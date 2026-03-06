{ nixpkgs, pulls, ... }:

let
  lib = (import nixpkgs { system = "x86_64-linux"; }).lib;

  mkJobset = { branch, description, shares ? 100 }: {
    enabled = 1;
    hidden = false;
    inherit description;

    type = 1;
    flake = "github:oliverpauffley/initiative/${branch}";

    checkinterval = 60;
    schedulingshares = shares;
    enableemail = false;
    keepnr = 3;
  };

  mainJobset = {
    main = mkJobset {
      branch = "main";
      description = "initiative – main branch";
      shares = 200;
    };
  };

  prJobsets = lib.mapAttrs' (num: pr:
    lib.nameValuePair "pr-${num}" (mkJobset {
      branch = pr.head.ref;
      description = "PR #${num}: ${pr.title}";
      shares = 50;
    })) pulls;

in mainJobset // prJobsets
