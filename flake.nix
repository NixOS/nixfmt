# © 2019 Serokell <hi@serokell.io>
# © 2019 Lars Jellema <lars.jellema@gmail.com>
#
# SPDX-License-Identifier: MPL-2.0
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
      }
    );
}
