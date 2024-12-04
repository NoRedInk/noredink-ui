# Elm-review overlay

## Why?

Newer elm-review doesn't download elm-json on-demand, so it won't flake on CI as much.

Newer elm-review wasn't on nixpkgs unstable yet.

## How to update this?

```sh
nix-shell -p node2nix --run 'node2nix --nodejs-18 -i node-packages.json -o node-packages.nix -c node-composition.nix'
```
