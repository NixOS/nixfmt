# `nixfmt`

`nixfmt` is the official formatter for Nix language code, intended to easily apply a uniform style.

![Build Status](https://github.com/NixOS/nixfmt/actions/workflows/main.yml/badge.svg?branch=master)

## State

`nixfmt` was originally developed by [Serokell](https://github.com/serokell).
It was used as the basis for the official standardised Nix formatter, as established by [RFC 166](https://github.com/NixOS/rfcs/pull/166).

The official standard differs considerably from the original implementation.
Be aware of this if you track the [`master`](https://github.com/NixOS/nixfmt/tree/master) branch.
Until the [next release](https://github.com/NixOS/nixfmt/issues/272), `nixfmt` should be considered **very unstable**.

A recent version of `nixfmt` is available as `pkgs.nixfmt-rfc-style` in Nixpkgs.
The original `nixfmt` is also still available as `pkgs.nixfmt-classic` for now, though unmaintained.

For more details, see the [RFC implementation tracking issue](https://github.com/NixOS/nixfmt/issues/153).

## Installation And Usage Instructions

### nixpkgs/NixOS

`nixfmt` was used as the basis for the official Nix formatter with a standardized formatting.
The new formatting differs considerably from the original one.
A recent nixfmt version is available as `pkgs.nixfmt-rfc-style` in Nixpkgs.
The formatting of this version differs considerably from the original nixfmt that was used as the basis for the standardised official formatter, which is also still available as `pkgs.nixfmt-classic` for now, though unmaintained.

So installing this `nixfmt` is as simple as adding to the system packages:

```nix
{ pkgs, ... }:

{
  environment.systemPackages = [ pkgs.nixfmt-rfc-style ];
}
```

### From the repository

It's also possible to install `nixfmt` directly from the repository using `nix-env`.
Also, updates are not done automatically (as it would with the system packages).

```console
$ nix-env -i -f https://github.com/NixOS/nixfmt/archive/master.tar.gz
```

### nix fmt

[nix fmt](https://nix.dev/manual/nix/latest/command-ref/new-cli/nix3-fmt) (which is a flakes-only feature) can be configured by adding the following to `flake.nix` (assuming a `nixpkgs` input exists):
```nix
{
  outputs =
    { nixpkgs, self }:
    {
      formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.nixfmt-rfc-style;
    };
}
```

### treefmt

[treefmt](https://github.com/numtide/treefmt) can be used to format repositories consisting of different languages with one command.
A possible configuration for `nixfmt` in `treefmt.toml` looks like this:
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

More information about configuration can be found in [the README](https://github.com/numtide/treefmt-nix?tab=readme-ov-file#integration-into-nix).

### git-hooks.nix

[git-hooks.nix](https://github.com/cachix/git-hooks.nix) can automatically configure git hooks like `pre-commit` using nix configuration and has native support for `nixfmt`:

```nix
{
  pre-commit-check = nix-pre-commit-hooks.run {
    # ... other options ...
    hooks = {
      # ... other hooks ...
      nixfmt-rfc-style.enable = true;
    };
  };
}
```

### `pre-commit` tool

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

### git mergetool

Nixfmt provides a mode usable by [`git mergetool`](https://git-scm.com/docs/git-mergetool)
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
