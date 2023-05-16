<!-- © 2019 Serokell <hi@serokell.io>
   - © 2019 Lars Jellema <lars.jellema@gmail.com>
   -
   - SPDX-License-Identifier: MPL-2.0
   -->

# `nixfmt`

`nixfmt` is a formatter for Nix code, intended to easily apply a uniform style.

You are encouraged to test this out on your code and submit any undesirable formatting you find as an issue

[![Build Status](https://badge.buildkite.com/b37f73adea391439e63288e8fd3b47f4b98fb9640bb864ccfa.svg)](https://buildkite.com/serokell/nixfmt)
[![Cachix](https://img.shields.io/badge/cachix-nixfmt-blue.svg)](https://nixfmt.cachix.org)

## Installation

- `nixfmt` is in nixpkgs master as of 2019-09-04: 

      nix-env -iA nixpkgs.nixfmt

- To get the most recent version, install from master:

      # Optional: use cachix to get cached builds even for master
      cachix use nixfmt
      nix-env -f https://github.com/serokell/nixfmt/archive/master.tar.gz -i

- Nix with flakes

      nix profile install github:serokell/nixfmt

## Development

### With Nix

Haskell dependencies will be built by Nix.

* Enter `nix-shell`
* Build with `cabal new-build`

### Without Nix

Haskell dependencies will be built by Cabal.

* Build with `cabal new-build`


## Usage

* `nixfmt < input.nix` – reads Nix code from `stdin`, formats it, and outputs to `stdout`
* `nixfmt file.nix` – format the file in place


## For Contributors

We welcome issues and pull requests on GitHub.


## About Serokell

`nixfmt` is maintained and funded with :heart: by
[Serokell](https://serokell.io/). The names and logo for Serokell are trademark
of Serokell OÜ.

We love open source software! See
[our other projects](https://serokell.io/community?utm_source=github) or
[hire us](https://serokell.io/hire-us?utm_source=github) to design, develop and
grow your idea!
