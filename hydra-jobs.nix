{ nixpkgs ? <nixpkgs>, system ? "x86_64-linux" }:

let
  overlay = pkgsNew: pkgsOld: {
    initiative = pkgsNew.haskell.lib.justStaticExecutables
      pkgsNew.haskellPackages.initiative;

    haskellPackages = pkgsOld.haskellPackages.override (old: {
      overrides = hself: hsuper: {
        initiative = pkgsNew.haskell.lib.overrideCabal
          (hself.callPackage ./initiative.nix { }) (drv: { });
      };
    });
  };

  pkgs = import nixpkgs { inherit system; overlays = [ overlay ]; };

in {
  build = pkgs.initiative;

  dockerImage = pkgs.dockerTools.buildLayeredImage {
    name = "initiative-image";
    tag = "latest";
    contents = [ pkgs.initiative ];
    config.Cmd = [ "${pkgs.initiative}/bin/initiative" ];
  };

  integration-tests = pkgs.nixosTest {
    name = "initiative-integration-tests";

    nodes.machine = { config, pkgs, ... }: {
      services.postgresql = {
        enable = true;
        authentication = ''
          local all all              trust
          host  all all 127.0.0.1/32 trust
          host  all all ::1/128       trust
        '';
        initialScript = pkgs.writeText "init.sql" ''
          create role dev_role nologin;
          create role dev inherit login password 'dev-password';
          grant dev_role to dev;
          create database initiative;
          \connect initiative
          grant all on schema public to dev_role;
        '';
      };
    };

    testScript = ''
      machine.wait_for_unit("postgresql.service")
      machine.wait_for_open_port(5432)
      machine.succeed(
        "${pkgs.initiative}/bin/initiative-integration-tests"
      )
    '';
  };
}
