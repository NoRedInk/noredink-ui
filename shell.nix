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
    niv.niv
    jq

    # building with Shake
    haskellPackages.ormolu
    (haskellPackages.ghcWithPackages (ps: [ ps.shake ]))

    # node dependencies
    nodejs
    nodePackages.npm

    # elm dependencies
    elmPackages.elm
    elmPackages.elm-format
    elmPackages.elm-test
    elmPackages.elm-verify-examples

    # preview dependencies
    entr
    python3

    # stuff we need for running builds in a `nix-shell --pure` environment.
    which
  ] ++ lib.optionals stdenv.isLinux [ pkgs.fsatrace pkgs.strace pkgs.cacert ];
}
