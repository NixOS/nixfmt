<!-- © 2019 Serokell <hi@serokell.io>
   - © 2019 Lars Jellema <lars.jellema@gmail.com>
   -
   - SPDX-License-Identifier: MPL-2.0
   -->

# `nixfmt`

`nixfmt` is a formatter for Nix code, intended to easily apply a uniform style.

You are encouraged to test this out on your code and submit any undesirable formatting you find as an issue

![Build Status](https://github.com/serokell/nixfmt/actions/workflows/main.yml/badge.svg?branch=master)

## State

`nixfmt` will form the basis for the initial official standard Nix formatter, as established by [RFC 166](https://github.com/NixOS/rfcs/pull/166).

The established standard Nix formatting differs considerably from the original one. Be aware of this if you track the main branch. Until the first new release the main branch should be considered **very unstable**.

For more details, see the [RFC implementation tracking issue](https://github.com/serokell/nixfmt/issues/153).

## Installation

- `nixfmt` is in nixpkgs master as of 2019-09-04: 

      nix-env -iA nixpkgs.nixfmt

- To get the most recent version, install from master:

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


## About Serokell

`nixfmt` is maintained and funded with :heart: by
[Serokell](https://serokell.io/). The names and logo for Serokell are trademark
of Serokell OÜ.

We love open source software! See
[our other projects](https://serokell.io/community?utm_source=github) or
[hire us](https://serokell.io/hire-us?utm_source=github) to design, develop and
grow your idea!
