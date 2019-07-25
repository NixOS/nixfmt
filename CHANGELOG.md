<!-- © 2019 Serokell <hi@serokell.io>
   - © 2019 Lars Jellema <lars.jellema@gmail.com>
   -
   - SPDX-License-Identifier: MPL-2.0
   -->


# Revision history for nixfmt

## 0.2.0 -- 2019-07-25

* Fixed indentation of binders and some other expressions.
* Use atomic writes to avoid data loss.
* Made idempotent.
* Pinned nixpkgs.
* Simplified some code.
* Many other formatting improvements.

## 0.1.0 -- 2019-05-11

* The first released version of nixfmt. This project aims to provide a
  consistent formatter for Nix code. This release is capable of parsing all of
  nixpkgs and formatting it in a consistent way. The generated code is not yet
  pretty in all places though and there are still some other issues as well.
