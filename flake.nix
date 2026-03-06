{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-25.05";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, utils, self, ... }:
    (utils.lib.eachDefaultSystem (system:
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

        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };

      in {

        packages.default = pkgs.initiative;

        packages.dockerImage = pkgs.dockerTools.buildLayeredImage {
          name = "initiative-image";
          tag = "latest";
          contents = [ pkgs.initiative ];
          config.Cmd = [ "${pkgs.initiative}/bin/initiative" ];
        };

        apps.default = {
          type = "app";
          program = "${pkgs.initiative}/bin/initiative";
        };

        checks.integration-tests = pkgs.nixosTest {
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

        devShells.default = pkgs.mkShell {
          inputsFrom = [ pkgs.haskellPackages.initiative.env ];
          packages = with pkgs; [ postgresql ];
        };
      })) // {
        nixosModules.default = { config, lib, pkgs, ... }:
          let cfg = config.services.initiative;
          in {
            options.services.initiative = {
              enable = lib.mkEnableOption "initiative TTRPG scheduling service";

              package = lib.mkOption {
                type = lib.types.package;
                default = pkgs.initiative;
                description = "The initiative package to use.";
              };

              configFile = lib.mkOption {
                type = lib.types.path;
                description = ''
                  Path to the config.toml file. Contains secrets (DB credentials,
                  OAuth client secret) so must be managed outside the Nix store
                  — use sops-nix, agenix, or similar.
                '';
                example = "/run/secrets/initiative/config.toml";
              };

              port = lib.mkOption {
                type = lib.types.port;
                default = 8080;
                description =
                  "Port the HTTP server listens on (currently hardcoded in the binary).";
              };
            };

            config = lib.mkIf cfg.enable {
              users.users.initiative = {
                isSystemUser = true;
                group = "initiative";
                description = "initiative service user";
              };
              users.groups.initiative = { };

              systemd.services.initiative = {
                description = "initiative TTRPG scheduling service";
                wantedBy = [ "multi-user.target" ];
                after = [ "network.target" "postgresql.service" ];
                serviceConfig = {
                  ExecStart = "${cfg.package}/bin/initiative";
                  WorkingDirectory = "/run/initiative";
                  RuntimeDirectory = "initiative";
                  User = "initiative";
                  Group = "initiative";
                  # Bind-mount the secrets config as config.toml in the working dir
                  BindPaths =
                    [ "${cfg.configFile}:/run/initiative/config.toml" ];
                  NoNewPrivileges = true;
                };
              };
            };
          };
      };
}
