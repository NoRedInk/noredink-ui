let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
  niv = import sources.niv { };
in with nixpkgs;

stdenv.mkDerivation {
  name = "noredink-ui";
  buildInputs = [
    # base dependencies
    git
    gnumake
    niv.niv
    jq

    # node dependencies
    nodejs
    nodePackages.npm

    # preview dependencies
    entr
    python3
  ];
}
