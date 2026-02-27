{ nixpkgs ? <nixpkgs> }:

let
  pkgs = import nixpkgs { };
  hlib = pkgs.haskell.lib;
in {

  initiative =
    hlib.disableCheck (pkgs.haskellPackages.callPackage ./initiative.nix { });
}
