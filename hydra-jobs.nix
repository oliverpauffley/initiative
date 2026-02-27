{ nixpkgs, src, ... }:

let
  pkgs = import nixpkgs { };
  hlib = pkgs.haskell.lib;
  initiative = pkgs.haskellPackages.callPackage ./initiative.nix { };
in rec {
  build = hlib.overrideCabal initiative
    (old: { testTarget = "initiative-unit-tests"; });

  integration-tests = pkgs.nixosTest {
    name = "initiative-integration-tests";

    nodes.machine = { config, pkgs, ... }: {
      services.postgresql = {
        enable = true;
        ensureDatabases = [ "initiative" ];
        ensureUsers = [{
          name = "initiative";
          ensureDBOwnership = true;
        }];
        authentication = ''
          local all all trust
          host  all all 127.0.0.1/32 trust
        '';
      };

      inputsFrom = [ initiative ];
    };

    testScript = ''
            machine.wait_for_unit("postgresql.service")
            machine.wait_for_open_port(5432)

      machine.succeed(
              "cabal test all 2>&1"
            )
    '';
  };
}
