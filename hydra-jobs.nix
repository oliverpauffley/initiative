{ nixpkgs, src, ... }:

let
  pkgs = import nixpkgs { };
  hlib = pkgs.haskell.lib;
  initiative = pkgs.haskellPackages.callPackage ./initiative.nix { };

  role = "dev_role";
  schema = "initiative";
  username = "dev";
  password = "dev-password";
in rec {

  integration-tests = pkgs.nixosTest {
    name = "initiative-integration-tests";

    nodes.machine = { config, pkgs, ... }: {
      services.postgresql = {
        enable = true;
        ensureUsers = [{ name = "initiative"; }];
        authentication = ''
          local all all              trust
          host  all all 127.0.0.1/32 trust
          host  all all ::1/128       trust
        '';
        initialScript = pkgs.writeText "initialScript.sql" ''
          create schema ${schema};
          create role ${role} nologin;

          create role ${username} inherit login password '${password}';
          grant ${role} to ${username};

          create database ${schema};
          \connect initiative
          grant all on schema public to ${role};
        '';
      };
    };

    testScript = ''
      machine.wait_for_unit("postgresql.service")
      machine.wait_for_open_port(5432)

      machine.succeed(
        "${initiative}/bin/initiative-integration-tests"
      )
    '';
  };

  build = initiative;
}
