<!-- © 2019 Serokell <hi@serokell.io>
   - © 2019 Lars Jellema <lars.jellema@gmail.com>
   -
   - SPDX-License-Identifier: MPL-2.0
   -->


# `nixfmt`

`nixfmt` is a formatter for Nix code, intended to easily apply a uniform style.
This is a work in progress.


## Installation

* `nix-env -f https://github.com/serokell/nixfmt/archive/master.tar.gz -i`


## Development

### With Nix

Haskell dependencies will be built by Nix.

* Enter `nix-shell`
* Build with `cabal new-build`

### Without Nix

Haskell dependencies will be bulit by Cabal.

* Build with `cabal new-build`


## Usage

* `nixfmt < input.nix` – reads Nix code form `stdin`, formats it, and outputs to `stdout`.
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
