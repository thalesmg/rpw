{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.Async     (withAsync)
import           Control.Concurrent.Chan      (Chan, newChan, readChan,
                                               writeChan)
import           Control.Exception            (SomeException, bracket_, handle)
import           Control.Monad                (forever, void)
import qualified Data.ByteString.Char8        as C8
import           System.Console.Terminal.Size (Window (..), size)
import           System.Environment           (getArgs, getEnvironment)
import           System.Exit                  (exitFailure)
import           System.IO                    (BufferMode (..), Handle,
                                               hGetEcho, hSetBuffering,
                                               hSetEcho, stdin, stdout)
import           System.Posix.IO              (FdOption (..), OpenMode (..),
                                               defaultFileFlags, dupTo,
                                               fdToHandle, openFd, setFdOption,
                                               stdError, stdInput, stdOutput)
import           System.Posix.Process         (createSession, executeFile,
                                               forkProcess, getProcessStatus)
import           System.Posix.Pty             (createPty, resizePty)
import           System.Posix.Signals         (signalProcess,
                                               softwareTermination)
import           System.Posix.Terminal        (getTerminalName,
                                               openPseudoTerminal)
import           System.Posix.Types           (Fd (..))
import qualified Text.Regex.Posix.ByteString  as R

data CtlSignal = CtlSignal
data SyncSignal = SyncSignal

withoutEcho :: IO () -> IO ()
withoutEcho act = do
  oldEcho <- hGetEcho stdin
  bracket_ (hSetEcho stdin False) (hSetEcho stdin oldEcho) act

slave :: Fd -> [(String, String)] -> String -> [String] -> IO ()
slave slaveFd env cmd args = do
  ptsSlave <- getTerminalName slaveFd
  -- create a new session so the slave can become the controlling
  -- terminal and session leader.
  void createSession
  -- this is needed for job control and effectively opening the
  -- terminal.
  slaveFd' <- openFd ptsSlave ReadWrite Nothing defaultFileFlags
  void $ dupTo slaveFd' stdInput
  void $ dupTo slaveFd' stdOutput
  void $ dupTo slaveFd' stdError
  -- prepare slave terminal attributes
  fdToHandle slaveFd' >>= flip hSetBuffering NoBuffering
  void $ getProcessStatus True False =<< forkProcess (executeFile "/bin/sh" True ["-c", "stty brkint ignpar imaxbel isig icanon < " <> ptsSlave] (Just env))
  executeFile cmd True args (Just env)

supervisorLoop :: Handle -> C8.ByteString -> Maybe C8.ByteString -> R.Regex -> IO ()
supervisorLoop masterH buf mpass regex = do
  ctlChan <- newChan
  syncChan <- newChan
  void $ withAsync (writeLoop masterH buf mpass regex ctlChan syncChan) $ \_ -> do
    let go = withAsync (readLoop masterH) $ \_ -> readChan ctlChan
    forever $ do
      void go
      writeChan syncChan SyncSignal
      void $ readChan ctlChan

readLoop :: Handle -> IO ()
readLoop masterH = forever $ C8.hGetSome stdin 4096 >>= C8.hPutStr masterH

writeLoop :: Handle
          -> C8.ByteString
          -> Maybe C8.ByteString
          -> R.Regex
          -> Chan CtlSignal
          -> Chan SyncSignal
          -> IO ()
writeLoop masterH buf mpass regex ctlChan syncChan = do
  c <- C8.hGetSome masterH 4096
  C8.hPutStr stdout c
  let buf' =
        if c == "\n"
          then ""
          else buf <> c
  (buf'', mpass') <-
    R.execute regex buf' >>= \case
      Right (Just _) ->
        case mpass of
          Just pass -> do
            C8.hPutStr stdout "<- (rpw..cached) "
            C8.hPutStrLn masterH pass
            pure ("", Just pass)
          Nothing -> do
            C8.hPutStr stdout "(rpw..sudo) <- "
            -- stop the reader so it doesn't gobble up the password
            writeChan ctlChan CtlSignal
            -- wait for it to be stopped
            void $ readChan syncChan
            pass <- C8.hGetLine stdin
            C8.hPutStrLn masterH pass
            -- start the reader again.
            writeChan ctlChan CtlSignal
            pure ("", Just pass)
      _ -> pure (buf', mpass)
  writeLoop masterH buf'' mpass' regex ctlChan syncChan

main :: IO ()
main = do
  env <- getEnvironment
  args' <- getArgs
  (cmd, args) <-
    case args' of
      [] -> do
        putStrLn "usage: rpw COMMAND ARG0 ARG1 ..."
        exitFailure
      (cmd:args) -> pure (cmd, args)
  Just (Window height width) <- size
  (masterFd, slaveFd) <- openPseudoTerminal
  masterH <- fdToHandle masterFd
  hSetBuffering masterH NoBuffering
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  setFdOption masterFd CloseOnExec True
  Right regex <-
    R.compile
      R.compExtended
      R.execBlank
      "(\\[sudo\\] password for [0-9a-zA-Z_]+: |SUDO password: )"
  pid <- forkProcess (slave slaveFd env cmd args)
  -- no need for echo: our slave will tell us what to print
  handle
    ((\_ -> signalProcess softwareTermination pid) :: SomeException -> IO ()) $
    withoutEcho $
      withAsync (supervisorLoop masterH "" Nothing regex) $ \_ -> do
      -- resize the terminal
      Just pty <- createPty masterFd
      resizePty pty (width, height)
      -- forward C-c to slave
      masterPts <- getTerminalName stdInput
      void $ getProcessStatus True False =<< forkProcess (executeFile "/bin/sh" True ["-c", "stty icrnl -ignpar -imaxbel -brkint -ixon -iutf8 -isig -opost -onlcr -iexten -echo -echoe -echok -echoctl -echoke < " <> masterPts] (Just env))
      -- void $ getProcessStatus True False =<< forkProcess (executeFile "/bin/sh" True ["-c", "stty sane < " <> masterPts] (Just env))
      -- wait and cleanup
      void $ getProcessStatus True False pid
