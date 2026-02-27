{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-25.05";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, utils, self, ... }:
    utils.lib.eachDefaultSystem (system:
      let
        config = { };

        overlay = pkgsNew: pkgsOld: {
          initiative = pkgsNew.haskell.lib.justStaticExecutables
            pkgsNew.haskellPackages.initiative;

          haskellPackages = pkgsOld.haskellPackages.override (old: {
            overrides = hself: hsuper: {
              initiative = pkgsNew.haskell.lib.overrideCabal
                (hself.callPackage ./initiative.nix { })
                (drv: { testTarget = "initiative-unit-tests"; });
            };
          });
        };

        pkgs = import nixpkgs {
          inherit config system;
          overlays = [ overlay ];
        };

      in rec {

        packages.default = pkgs.initiative;

        packages.dockerImage = pkgs.dockerTools.buildLayeredImage {
          name = "initiative-image";
          tag = "latest";

          contents = [ pkgs.initiative ];
          config = { Cmd = [ "${pkgs.initiative}/bin/initiative" ]; };
        };

        apps.default = {
          type = "app";
          program = "${pkgs.initiative}/bin/initiative";
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [ pkgs.haskellPackages.initiative.env ];
          packages = with pkgs; [ postgresql ];
        };
      });
}
