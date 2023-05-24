let
  sources = import ./nix/sources.nix;
  system = if builtins.currentSystem == "aarch64-darwin" then
    "x86_64-darwin"
  else
    builtins.currentSystem;
  nixpkgs = import sources.nixpkgs { inherit system; };
  niv = import sources.niv { };
  awsudo = nixpkgs.writeShellScriptBin "awsudo" ''
    exec ${nixpkgs.aws-vault}/bin/aws-vault exec \
      --duration="''${SUDO_DURATION:-1h}" "''${SUDO_ROLE:-sudo}" -- "$@"'';
in with nixpkgs;
stdenv.mkDerivation {
  name = "noredink-ui";
  buildInputs = [
    # base dependencies
    awsudo
    git
    niv.niv
    jq
    zstd

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
    elmPackages.elm-language-server
    elmPackages.elm-verify-examples
    elmPackages.elm-review
    elmPackages.elm-json
    (pkgs.callPackage sources.elm-forbid-import { })

    # preview dependencies
    python3
    watchexec

    # stuff we need for running builds in a `nix-shell --pure` environment.
    which
    netcat-gnu

    # Buck dependencies
    awscli2
    jwt-cli
    black
    buildifier
  ] ++ lib.optionals stdenv.isLinux [ pkgs.fsatrace pkgs.strace pkgs.cacert ];
}
