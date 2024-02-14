{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    elm-forbid-import.url = "git+https://git.bytes.zone/brian/elm-forbid-import.git";
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import inputs.nixpkgs { inherit system; };
      in {
        formatter = pkgs.nixpkgs-fmt;

        devShell = pkgs.mkShell {
          packages = [
            # base dependencies
            pkgs.git
            pkgs.jq
            pkgs.zstd

            # building with Shake
            pkgs.haskellPackages.ormolu
            (pkgs.haskellPackages.ghcWithPackages (ps: [ ps.shake ]))

            # node dependencies
            pkgs.nodejs
            pkgs.nodePackages.npm

            # elm dependencies
            pkgs.elmPackages.elm
            pkgs.elmPackages.elm-format
            pkgs.elmPackages.elm-test
            pkgs.elmPackages.elm-language-server
            pkgs.elmPackages.elm-verify-examples
            pkgs.elmPackages.elm-review
            pkgs.elmPackages.elm-json
            inputs.elm-forbid-import.defaultPackage.${system}

            # preview dependencies
            pkgs.python3
            pkgs.watchexec
            pkgs.elmPackages.elm-live

            # stuff we need for running builds in a `nix-shell --pure` environment.
            pkgs.which
            pkgs.netcat-gnu

            # Buck dependencies
            pkgs.black
            pkgs.buildifier
          ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ pkgs.fsatrace pkgs.strace pkgs.cacert ];
        };
      }
    );
}