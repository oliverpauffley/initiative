{ nixpkgs ? <nixpkgs> }:

let pkgs = import nixpkgs { };
in { initiative = pkgs.haskellPackages.callPackage ./initiative.nix { }; }
