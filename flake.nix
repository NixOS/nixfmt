{

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs =
    { flake-utils, self }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        result = import ./default.nix { inherit system; };
      in
      {
        packages = result.packages // {
          default = result;
        };

        apps.default = {
          type = "app";
          program = "${result}/bin/nixfmt";
        };

        checks = result.checks;

        devShells.default = result.shell;
      }
    );
}
