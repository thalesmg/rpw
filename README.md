# rpw [![Build Status](https://travis-ci.org/thalesmg/rpw.svg?branch=master)](https://travis-ci.org/thalesmg/rpw)

This is a Haskell implementation of [`cache-pw`](https://gitlab.com/dgvncsz0f/dot/blob/master/roles/bash/files/bin/cache-pw).

![](./demo.gif)

## How to install

You need [Stack](https://docs.haskellstack.org/en/stable/README/).

```bash
$ stack install
```

## How to use

```bash
$ rpw bash
$ sudo echo hello!
[sudo] password for foobar: (rpw..sudo) <-        # type your password and
                                                  # end with `Enter`, `C-j` or `C-m`
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

## Testing

You need `docker` and `docker-compose` for this:

```bash
make test
```

## TODO

- [ ] Allow resetting the password without exiting the shell.
- [ ] Emit `SIGWINCH` to the slave terminal when needed.
