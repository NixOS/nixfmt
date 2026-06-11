{
  description = "The official formatter for Nix language code";

  inputs.systems = {
    type = "github";
    owner = "nix-systems";
    repo = "default";
    flake = false;
  };

  inputs.nixpkgs = {
    type = "github";
    owner = "NixOS";
    repo = "nixpkgs";
    ref = "nixpkgs-unstable";
  };

  inputs.treefmt-nix = {
    type = "github";
    owner = "numtide";
    repo = "treefmt-nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  inputs.flake-compat = {
    type = "github";
    owner = "NixOS";
    repo = "flake-compat";
    flake = false;
  };

  outputs =
    inputs:
    let
      systems = import inputs.systems;

      genAttrs =
        names: fn:
        builtins.listToAttrs (
          map (name: {
            inherit name;
            value = fn name;
          }) names
        );

      perSystem = genAttrs systems;

      results = perSystem (system: import ./main.nix {
        inherit system;
        inherit (inputs) nixpkgs treefmt-nix;
      });
      mapResults = fn: builtins.mapAttrs (_: fn) results;
    in
    {
      packages = mapResults (result: result.packages // { default = result.packages.nixfmt; });

      checks = mapResults (result: result.checks);

      devShells = mapResults (result: {
        default = result.shell;
      });

      formatter = mapResults (result: result.treefmt);
    };
}
