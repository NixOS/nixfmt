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
      inherit (inputs.nixpkgs) lib;

      call =
        f:
        let
          fn = if lib.isPath f then import f else f;
          autoArgs = lib.intersectAttrs (lib.functionArgs fn) inputs;
        in
        args: fn (autoArgs // args);

      systems = import inputs.systems;
      perSystem = lib.genAttrs systems;

      results = perSystem (system: call ./main.nix { inherit system; });
      mapResults = fn: lib.mapAttrs (_: fn) results;
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
