Tests in `correct` are formatted the way they should be.
Running `nixfmt --verify` on them should result in the output being the same as
the input.

Tests in `invalid` should return an error when formatted.

Tests in `diff` have input and output files. `nixfmt --verify < foo.in.nix`
should output exactly `foo.out.nix`.

`test.sh` runs these tests for you. Pass `--update-diff` to update the out files in `diff`.
