# Revision history for nixfmt

## 1.0.1 -- 2025-09-16

- Fix bug where `--ir` would overwrite the source file: <https://github.com/NixOS/nixfmt/pull/322>
- Fix `pre-commit` hook so it works without cabal: <https://github.com/NixOS/nixfmt/pull/311>

## 1.0.0 -- 2025-07-09

The [Nix Formatting Team](https://nixos.org/community/teams/formatting/) is happy to present the first stable release of the official Nix formatter! The basis for this milestone is [RFC 166](https://github.com/NixOS/rfcs/pull/166), which defined the [standard for Nix formatting](https://github.com/NixOS/nixfmt/blob/master/standard.md), established the Nix Formatting team and set the groundwork for nixfmt to become the official formatter.

Given that, this release is _significantly_ different from the previous one:
- How Nix is formatted _completely_ changed and is unrecognisable from previous versions, fixing many issues with the old formatting in the process. It would be pointless to try to list all the differences, just think of it as an entirely new formatting.
- This project graduated from a [Serokell](https://serokell.io/) project to an official Nix project, with a [repository under the NixOS org](https://github.com/nixos/nixfmt) and a community-based Nix formatting team as maintainers.

Other than the above, there are some notable UX changes:
- Deprecate `nixfmt [dir]` for recursively formatting a directory again. Please use the new `pkgs.nixfmt-tree` wrapper instead, or <https://github.com/numtide/treefmt-nix> for more flexibility, see [the docs](https://github.com/nixos/nixfmt?tab=readme-ov-file#in-a-project) for more options.
- More complete [usage documentation](https://github.com/nixos/nixfmt?tab=readme-ov-file#installation).
  - A [Git mergetool mode](https://github.com/nixos/nixfmt?tab=readme-ov-file#git-mergetool) is now supported.
- CLI changes:
  - In stdin-mode, `--filename <path>` can now be used to specify a filename for diagnostics.
  - Number of indendation spaces can now be configured using `--indent <number>`

## 0.6.0 -- 2023-10-31

* Fix escaping of interpolations after dollar signs.
* Fix nixfmt trying to allocate temp files that aren't used.
* Don't write if files didn't change, fixing treefmt compatibility
* Nixfmt now accepts the '-' argument to read from stdin.
* `nixfmt [dir]` now recursively formats nix files in that directory.
* Float and int literal parsing now matches nix.

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
