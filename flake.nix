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
            overrides =
              pkgsNew.haskell.lib.packageSourceOverrides { initiative = ./.; };
          });
        };

        pkgs = import nixpkgs {
          inherit config system;
          overlays = [ overlay ];
        };

      in rec {
        packages.default = pkgs.haskellPackages.initiative;

        apps.default = {
          type = "app";

          program = "${pkgs.initiative}/bin/initiative";
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [ pkgs.haskellPackages.initiative.env ];
          packages = with pkgs; [ postgresql ];
        };
        hydraJobs = { inherit (self) initiative; };
      });
}
