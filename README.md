# `nixfmt`

`nixfmt` is a formatter for Nix code, intended to easily apply a uniform style.
This is a work in progress.

## Build Instructions

First, enter `nix-shell`, then run `cabal v1-build` to build, `cabal v1-run` to
run and `cabal v1-repl` for a REPL.

## Usage

Run `nix-build` in the root of the project. The binary can then be called with
`result/bin/nixfmt`. It reads nix code from stdin and writes a formatted version
to stdout. Currently, many parts of the syntax are still unsupported.

## For Contributers

We welcome issues and pull requests on Github.

## About Serokell

`nixfmt` is maintained and funded with :heart: by
[Serokell](https://serokell.io/). The names and logo for Serokell are trademark
of Serokell OÜ.

We love open source software! See
[our other projects](https://serokell.io/community?utm_source=github) or
[hire us](https://serokell.io/hire-us?utm_source=github) to design, develop and
grow your idea!
