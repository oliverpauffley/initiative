{ nixpkgs, src, ... }:

let
  pkgs = import nixpkgs { };
  hlib = pkgs.haskell.lib;
in {
  initiative =
    hlib.overrideCabal (pkgs.haskellPackages.callPackage ./initiative.nix { })
    (old: { testTarget = "initiative-unit-tests"; });
}
