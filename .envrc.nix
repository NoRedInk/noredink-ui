use flake || use nix
# circumvent https://github.com/direnv/direnv/issues/1345
mkdir -p $TMPDIR
export PATH="$(pwd)/node_modules/.bin:$PATH"
