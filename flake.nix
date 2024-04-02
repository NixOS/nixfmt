{
  outputs =
    { self, nixpkgs }:
    let
      forAllSystems = nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed;

      result = forAllSystems (system: import ./default.nix { inherit system; });
    in
    {
      packages = forAllSystems (
        system:
        let
          packages = result.${system}.packages;
        in
        packages // { default = packages.nixfmt; }
      );

      apps = forAllSystems (system: {
        default = {
          type = "app";
          program = nixpkgs.lib.getExe self.packages.${system}.default;
        };
      });

      checks = forAllSystems (system: result.${system}.checks);

      devShells = forAllSystems (system: {
        default = result.${system}.shell;
      });
    };
}
