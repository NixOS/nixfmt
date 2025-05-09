# `nixfmt`

`nixfmt` is the official formatter for Nix language code, intended to easily apply a uniform style.

![Build Status](https://github.com/NixOS/nixfmt/actions/workflows/main.yml/badge.svg?branch=master)

## State

`nixfmt` was originally developed by [Serokell](https://github.com/serokell).
It was used as the basis for the official standardised Nix formatter, as established by [RFC 166](https://github.com/NixOS/rfcs/pull/166).

The official standard differs considerably from the original implementation.
Be aware of this if you track the [`master`](https://github.com/NixOS/nixfmt/tree/master) branch.
Until the [next release](https://github.com/NixOS/nixfmt/issues/272), expect `nixfmt` to change.

A recent version of `nixfmt` is available as `pkgs.nixfmt-rfc-style` in Nixpkgs.
The original `nixfmt` is still available as `pkgs.nixfmt-classic`, but it is unmaintained and will eventually be removed.

For more details, see the [RFC implementation tracking issue](https://github.com/NixOS/nixfmt/issues/153).

## Installation

> [!NOTE]
> `nixfmt` can only process one file at a time.
> Consider using a configuration helper for [formatting a project](#in-a-project).

### In the environment

#### NixOS

To install `nixfmt` on NixOS for all users, add it to the [`environment.systemPackages`](https://search.nixos.org/options?show=environment.systemPackages) configuration option:

```nix
{ pkgs, ... }:
{
  environment.systemPackages = [ pkgs.nixfmt-rfc-style ];
}
```

To install it on NixOS for a particular user, add it to the [`users.users.<user>.packages`](https://search.nixos.org/options?show=users.users.%3Cname%3E.packages) configuration option:

```
{ pkgs, ... }:
{
  users.users.example-user-name.packages = [ pkgs.nixfmt-rfc-style ];
}
```

#### Home Manager

To install `nixfmt` in Home Manager, add it adding to the [`home.packages`](https://nix-community.github.io/home-manager/options.xhtml#opt-home.packages) configuration option:

```nix
{ pkgs, ... }:
{
  home.packages = [ pkgs.nixfmt-rfc-style ];
}
```

#### Declarative shell environment

To make `nixfmt` available in a shell environment invoked with [`nix-shell`](https://nix.dev/manual/nix/2.28/command-ref/nix-shell), add it to the `packages` argument of `mkShell`:

```nix
{ pkgs }:
pkgs.mkShellNoCC {
  packages = [ pkgs.nixfmt-rfc-style ];
}
```

### In an editor

#### neovim + nixd

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

> [!NOTE]
> This only works when `nixfmt` is available [in the environment](#in-the-environment).

#### neovim + nil

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

> [!NOTE]
> This only works when `nixfmt` is available [in the environment](#in-the-environment).

#### neovim + none-ls

```lua
local null_ls = require("null-ls")
null_ls.setup({
    sources = {
        null_ls.builtins.formatting.nixfmt,
    },
})
```

> [!NOTE]
> This only works when `nixfmt` is available [in the environment](#in-the-environment).

### In a project

#### `treefmt-nix`

[`treefmt-nix`](https://github.com/numtide/treefmt-nix) automatically configures the correct packages and formatters for [`treefmt`](https://github.com/numtide/treefmt) using the Nix language, and has native support for `nixfmt`:

```nix
{ pkgs, treefmt-nix }:
treefmt-nix.mkWrapper pkgs {
  programs.nixfmt.enable = true;
};
```

#### `treefmt`

[`treefmt`](https://github.com/numtide/treefmt) can also be used directly:

```toml
# treefmt.toml
[formatter.nixfmt-rfc-style]
command = "nixfmt"
includes = ["*.nix"]
```

> [!NOTE]
> This only works when `nixfmt` is available [in the environment](#in-the-environment).

#### `git-hooks.nix`

[`git-hooks.nix`](https://github.com/cachix/git-hooks.nix) can automatically configure Git hooks like [`pre-commit`](https://pre-commit.com/) using the Nix language, and has native support for `nixfmt`:

```nix
{ pkgs, git-hooks }:
{
  pre-commit-check = git-hooks.run {
    hooks = {
      nixfmt-rfc-style.enable = true;
    };
  };
  shell = pkgs.mkShellNoCC {
    packages = [ pre-commit-check.enabledPackages ];
    shellHook = ''
      ${pre-commit-check.shellHook}
    '';
  };
}
```

#### `pre-commit`

[`pre-commit`](https://pre-commit.com/) can also be used directly:

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

    ```yaml
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

#### `git mergetool`

`nixfmt` provides a mode usable by [`git mergetool`](https://git-scm.com/docs/git-mergetool)
via `--mergetool` that allows resolving formatting-related conflicts automatically in many cases.

It can be installed by any of these methods:

- For only for the current repo, run:
  ```
  git config mergetool.nixfmt.cmd 'nixfmt --mergetool "$BASE" "$LOCAL" "$REMOTE" "$MERGED"'
  git config mergetool.nixfmt.trustExitCode true
  ```
- For all repos with a mutable config file, run
  ```
  git config --global mergetool.nixfmt.cmd 'nixfmt --mergetool "$BASE" "$LOCAL" "$REMOTE" "$MERGED"'
  git config --global mergetool.nixfmt.trustExitCode true
  ```
- For all repos with a NixOS-provided config file, add this to your `configuration.nix`:
  ```nix
  programs.git.config = {
    mergetool.nixfmt = {
      cmd = "nixfmt --mergetool \"$BASE\" \"$LOCAL\" \"$REMOTE\" \"$MERGED\"";
      trustExitCode = true;
    };
  };
  ```
- For all repos with a home-manager-provided config file, add this to your `home.nix`:
  ```nix
  programs.git.extraConfig = {
    mergetool.nixfmt = {
      cmd = "nixfmt --mergetool \"$BASE\" \"$LOCAL\" \"$REMOTE\" \"$MERGED\"";
      trustExitCode = true;
    };
  };
  ```

Then, when `git merge` or `git rebase` fails, run
```
git mergetool -t nixfmt .
# or, only for some specific files
git mergetool -t nixfmt FILE1 FILE2 FILE3
```

and some `.nix` files will probably get merged automagically.

Note that files that `git` merges successfully even before `git mergetool`
will be ignored by \`git mergetool\`.

If you don't like the result, run
```
git restore --merge .
# or, only for some specific files
git restore --merge FILE1 FILE2 FILE3
```

to return back to the unmerged state.

#### `nix fmt` (experimental)

[nix fmt](https://nix.dev/manual/nix/latest/command-ref/new-cli/nix3-fmt) (part of the [`flakes` experimental feature](https://nix.dev/manual/nix/latest/development/experimental-features#xp-feature-flakes)) can be configured to use `nixfmt` by setting the `formatter` flake output to the respective package (assuming a `nixpkgs` flake input exists):

```nix
# flake.nix
{
  outputs =
    { nixpkgs, self }:
    {
      formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.nixfmt-rfc-style;
    };
}
```

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
