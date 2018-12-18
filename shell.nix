with import (builtins.fetchTarball rec {
  # grab a hash from here: https://nixos.org/channels/
  name = "nixpkgs-darwin-18.09pre153253.7e88992a8c7";
  url = "https://github.com/nixos/nixpkgs/archive/7e88992a8c7b2de0bcb89182d8686b27bd93e46a.tar.gz";
  # Hash obtained using `nix-prefetch-url --unpack <url>`
  sha256 = "1f6lf4addczi81hchqbzjlhrsmkrj575dmdjdhyl0jkm7ypy2lgk";
}) {};

stdenv.mkDerivation {
  name = "noredink-ui";
  buildInputs = [
    # base dependencies
    git
    gnumake

    # node dependencies
    nodePackages.npm
  ];
}
