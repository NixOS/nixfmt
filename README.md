<!-- Copyright Serokell OU <hi@serokell.io> -->
<!-- Copyright Lars Jellema <lars.jellema@gmail.com> -->
<!-- SPDX-License-Identifier: MPL-2.0 -->

# `nixfmt`

`nixfmt` is a formatter for Nix code, intended to easily apply a uniform style.
This is a work in progress.

## Build & Installation

This project can be built with nix. `nix-build https://github.com/serokell/nixfmt/archive/master.tar.gz`, `nix-env -iAf https://github.com/serokell/nixfmt/archive/master.tar.gz`.

## Development

First, enter `nix-shell`, then run `cabal v1-build` to build, `cabal v1-run` to
run and `cabal v1-repl` for a REPL.

## Usage

Run `nix-build` in the root of the project. The binary can then be called with
`result/bin/nixfmt`. It reads nix code from stdin and writes a formatted version
to stdout. Invoking the binary with an argument file will cause the file to be
formatted in place.

## For Contributors

We welcome issues and pull requests on Github.

## About Serokell

`nixfmt` is maintained and funded with :heart: by
[Serokell](https://serokell.io/). The names and logo for Serokell are trademark
of Serokell OÜ.

We love open source software! See
[our other projects](https://serokell.io/community?utm_source=github) or
[hire us](https://serokell.io/hire-us?utm_source=github) to design, develop and
grow your idea!
