self: super:
let nodePkgs = super.callPackage ./node-composition.nix { };
in { elm-review = nodePkgs.elm-review; }
