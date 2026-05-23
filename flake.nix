{
  inputs.systems = {
    type = "github";
    owner = "nix-systems";
    repo = "default";
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

      results = perSystem (system: import ./. { inherit system; });
      mapResults = fn: builtins.mapAttrs (_: fn) results;
    in
    {
      packages = mapResults (result: result.packages // { default = result; });

      checks = mapResults (result: result.checks);

      devShells = mapResults (result: {
        default = result.shell;
      });

      formatter = mapResults (result: result.treefmt);
    };
}
