# rpw

This is a Haskell implementation of [`cache-pw`](https://gitlab.com/dgvncsz0f/dot/blob/master/roles/bash/files/bin/cache-pw).

## How to install

You need [Stack](https://docs.haskellstack.org/en/stable/README/).

```bash
$ stack install
```

## How to use

```bash
$ rpw bash
$ sudo echo hello!
[sudo] password for foobar: (rpw..sudo) <-        # type your password and end with C-j
hello!
$ sudo -k
$ sudo echo hello!
[sudo] password for foobar: <- (rpw..cached)      # no need to type here!
hello!
$ ansible-playbook playbook.yml -K
SUDO password: <- (rpw..cached)                   # no need to type here!
 _____________________
< PLAY [Some playbook!] >
 ---------------------
        \   ^__^
         \  (oo)\_______
            (__)\       )\/\
                ||----w |
                ||     ||

```

## TODO

- [ ] Find out why we need to `C-j` to enter the password, or why `Enter` does not work. 🤔
- [ ] Allow resetting the password without exiting the shell.
- [ ] Emit `SIGWINCH` to the slave terminal when needed.
