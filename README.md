# `nixfmt`

`nixfmt` is a formatter for Nix code, intended to easily apply a uniform style.

You are encouraged to test this out on your code and submit any undesirable formatting you find as an issue

![Build Status](https://github.com/NixOS/nixfmt/actions/workflows/main.yml/badge.svg?branch=master)

## State

`nixfmt` will form the basis for the initial official standard Nix formatter, as established by [RFC 166](https://github.com/NixOS/rfcs/pull/166).

The established standard Nix formatting differs considerably from the original one. Be aware of this if you track the main branch. Until the first new release the main branch should be considered **very unstable**.

For more details, see the [RFC implementation tracking issue](https://github.com/NixOS/nixfmt/issues/153).

## Installation And Usage Instructions

### nixpkgs/NixOS

`nixfmt` was used as the basis for the official Nix formatter with a standardized formatting.
The new formatting differs considerably from the original one.
This is why two packages are in nixpkgs: `nixfmt-classic` with the non-standardized formatting and `nixfmt-rfc-style`.

So installing this `nixfmt` is as simple as adding to the system packages:

```nix
{ pkgs, ... }:

{
  environment.systemPackages = [ pkgs.nixfmt-rfc-style ];
}
```

### From the repository

It's also possible to install `nixfmt` directly from the repository using `nix-env`.
This allows the newest changes to be used, which may not be equal to the `nixfmt` version used to format nixpkgs, so this should be done with care!
Also, updates are not done automatically (as with the system packages).

```
nix-env -f https://github.com/NixOS/nixfmt/archive/master.tar.gz -i
```

### nix fmt

[nix fmt](https://nix.dev/manual/nix/latest/command-ref/new-cli/nix3-fmt) (which is a flakes-only feature) can be configured by adding the following to `flake.nix` (assuming a `nixpkgs` input exists):
```
{
  outputs = { nixpkgs, self }: {
    formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.nixfmt-rfc-style;
  };
}
```

### treefmt

[treefmt](https://github.com/numtide/treefmt) can be used to format repositories consisting of different languages with one command.
A possible configuration for `nixfmt` in `.treefmt.toml` looks like this:
```toml
[formatter.nixfmt-rfc-style]
command = "nixfmt"
includes = ["*.nix"]
```

This only works when `nixfmt-rfc-style` is installed (see above for installation instructions).

`treefmt` can be integrated into text editors and CI to ensure consistent formatting for all filetypes.

### treefmt-nix

[treefmt-nix](https://github.com/numtide/treefmt-nix) automatically configures the correct packages and formatters using a Nix configuration and has native support for `nixfmt`:

```nix
# ...
treefmt-nix.mkWrapper nixpkgs {
  # ... other options ...
  programs.nixfmt-rfc-style.enable = true;
}
```

More information about configuration can be found in [the official README](https://github.com/numtide/treefmt-nix?tab=readme-ov-file#integration-into-nix).

### neovim + nixd

```lua
local nvim_lsp = require("lspconfig")
nvim_lsp.nixd.setup({
   settings = {
      nixd = {
         formatting = {
            command = { "nixfmt" },
         },
      },
   },
})
```

This only works when `nixfmt-rfc-style` is installed (see above for installation instructions).

### neovim + nil

```lua
local nvim_lsp = require("lspconfig")
nvim_lsp.nil_ls.setup({
   settings = {
      ['nil'] = {
         formatting = {
            command = { "nixfmt" },
         },
      },
   },
})
```

This only works when `nixfmt-rfc-style` is installed (see above for installation instructions).

### neovim + none-ls

```lua
local null_ls = require("null-ls")
null_ls.setup({
    sources = {
        null_ls.builtins.formatting.nixfmt,
    },
})
```

This only works when `nixfmt-rfc-style` is installed (see above for installation instructions).

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

### With the `pre-commit` tool

If you have Nix files in a Git repo and you want to make sure that they’re formatted with `nixfmt`, then you can use the `pre-commit` tool from [pre-commit.com](https://pre-commit.com):

1. Make sure that you have the `pre-commit` command:

    ```console
    $ pre-commit --version
    pre-commit 3.7.1
    ```

2. Make sure that you’re in your Git repo:

    ```console
    $ cd <path-to-git-repo>
    ```

3. Make sure that the `pre-commit` tool is installed as a Git pre-commit hook:

    ```console
    $ pre-commit install
    pre-commit installed at .git/hooks/pre-commit
    ```

4. If you don’t already have one, then create a `.pre-commit-config.yaml` file.

5. Add an entry for the `nixfmt` hook to your `.pre-commit-config.yaml` file:

    ```
    repos:
        - repo: https://github.com/NixOS/nixfmt
          rev: <version>
          hooks:
                - id: nixfmt
    ```

    If you want to use a stable version of `nixfmt`, then replace `<version>` with a tag from this repo. If you want to use an unstable version of `nixfmt`, then replace `<version>` with a commit hash from this repo.

6. Try to commit a badly formatted Nix file in order to make sure that everything works.

> [!WARNING]
> `nixfmt`’s integration with the `pre-commit` tool is relatively new. At the moment, none of the stable releases of `nixfmt` can be used with the `pre-commit` tool. You’ll have to use an unstable version for the time being.


## About Serokell

`nixfmt` is maintained and funded with :heart: by
[Serokell](https://serokell.io/). The names and logo for Serokell are trademark
of Serokell OÜ.

We love open source software! See
[our other projects](https://serokell.io/community?utm_source=github) or
[hire us](https://serokell.io/hire-us?utm_source=github) to design, develop and
grow your idea!
