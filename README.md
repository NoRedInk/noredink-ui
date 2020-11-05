Branch to repro this error:

```
Status: Starting...
Shakefile.hs: getCurrentDirectory:getWorkingDirectory: resource exhausted (Too many open files)
```

How to repro:

```sh
$ nix-shell --pure
[nix-shell:..]$ git ls-files | entr shake "public"
```

