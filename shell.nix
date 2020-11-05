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

    # building with Shake
    (haskellPackages.ghcWithPackages (ps: [ ps.shake ]))

    # preview dependencies
    entr
    
    # stuff we need for running builds in a `nix-shell --pure` environment.
    which
  ] ++ lib.optionals stdenv.isLinux [ pkgs.fsatrace pkgs.strace pkgs.cacert ];
}
