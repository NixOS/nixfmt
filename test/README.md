<!-- © 2022 Serokell <hi@serokell.io>
   - © 2022 Lars Jellema <lars.jellema@gmail.com>
   -
   - SPDX-License-Identifier: MPL-2.0
   -->

Tests in `correct` are formatted the way they should be.
Running `nixfmt --verify` on them should result in the output being the same as
the input.

Tests in `invalid` should return an error when formatted.

Tests in `changed` have input and output files. `nixfmt --verify foo.in.nix`
should output exactly `foo.out.nix`.
