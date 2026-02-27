{ nixpkgs, pulls, ... }:
let
  pkgs = import nixpkgs { };
  prs = builtins.fromJSON (builtins.readFile pulls);

  mkJobset = { branch, description, shares ? 100 }: {
    enabled = 1;
    hidden = false;
    inherit description;
    nixexprinput = "src";
    nixexprpath = "hydra-jobs.nix";
    checkinterval = 300;
    schedulingshares = shares;
    enableemail = false;
    emailoverride = "";
    keepnr = 3;
    inputs = {
      src = {
        type = "git";
        value = "https://github.com/oliverpauffley/initiative.git ${branch}";
        emailresponsible = false;
      };
      nixpkgs = {
        type = "git";
        value = "https://github.com/NixOS/nixpkgs release-25.05";
        emailresponsible = false;
      };
    };
  };

  prJobsets = pkgs.lib.mapAttrs (num: info:
    mkJobset {
      branch = "pull/${num}/head";
      description = "PR #${num}: ${info.title}";
      shares = 20;
    }) prs;

  allJobsets = prJobsets // {
    main = mkJobset {
      branch = "main";
      description = "Build main";
    };
  };

  jobsetsJson = builtins.toJSON allJobsets;
in { jobsets = pkgs.writeText "jobsets.json" jobsetsJson; }
