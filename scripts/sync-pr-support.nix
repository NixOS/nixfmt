/*
  This file supports the ./sync-pr.sh script with some Nix code.
  Note that we are not using the nixfmt from the tree of this file,
  instead the nixfmt to use is passed into the necessary function.
  This way we don't need to rely on this internal file being stable.
*/
let
  pkgs = (import ../default.nix { }).pkgs;
  inherit (pkgs) lib;
in
{
  # Filters a repo into a store path only containing its Git-tracked Nix files
  repoNixFiles =
    { repo }:
    lib.fileset.toSource {
      root = repo;
      fileset = lib.fileset.intersection (lib.fileset.gitTracked repo) (
        lib.fileset.fileFilter (file: file.hasExt "nix") repo
      );
    };

  # Returns a derivation that contains the passed store path (e.g. from the above function)
  # but with all Nix files formatted with the given nixfmt
  formattedGitRepo =
    { storePath, nixfmtPath }:
    let
      nixfmt = (import nixfmtPath { }).packages.nixfmt;
    in
    pkgs.runCommand "formatted"
      {
        nativeBuildInputs = with pkgs; [
          treefmt
          nixfmt
        ];
        treefmtConfig = ''
          [formatter.nixfmt-rfc-style]
          command = "nixfmt"
          options = [ "--verify" ]
          includes = [ "*.nix" ]
        '';
        passAsFile = [ "treefmtConfig" ];
      }
      ''
        cp -r --no-preserve=mode ${builtins.storePath storePath} $out
        treefmt \
          --config-file "$treefmtConfigPath" \
          --tree-root "$out" \
          --no-cache
      '';
}
