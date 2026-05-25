# Mirror `main.nix`'s function arguments for nix-command `--arg` support
{
  system ? null,
  nixpkgs ? null,
}@args:
let
  result = import ./main.nix args;
in
result.packages.nixfmt // result
