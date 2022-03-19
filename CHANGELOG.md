<!-- © 2019 Serokell <hi@serokell.io>
   - © 2019 Lars Jellema <lars.jellema@gmail.com>
   -
   - SPDX-License-Identifier: MPL-2.0
   -->


# Revision history for nixfmt

## 0.5.0 -- 2022-03-15
* Add a nix flake to the nixfmt project.
* Add a --verify flag to check idempotency.
* Support nix path (`./${foo}.nix`) interpolations.
* Fix escaping of interpolations after single quotes.
* Fix handling of multiline strings with spaces in the last line.

## 0.4.0 -- 2020-02-10
* Report non-conforming files on the same line to aid line-oriented processing
* Fix help, summary, and version flag contents.
* Fix indentation of leading comments in parens

## 0.3.0 -- 2019-08-29

* Added check flag for use in CI.
* Added quiet flag to disable all output on stderr.
* Further improved indentation.
* Fixed bugs where Nix code with different semantics was emitted in some cases.

## 0.2.1 -- 2019-07-29

* Fixed missing linebreaks in set abstractions.

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
