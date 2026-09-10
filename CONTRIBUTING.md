# Contribution Guidelines

We welcome issues and pull requests at https://github.com/NixOS/nixfmt.
PRs that change the format should preferably be discussed in an issue first.

When contributing, please try to familiarize yourself with the [Nix Format Standard](https://github.com/NixOS/nixfmt/blob/master/standard.md), because the distinction between bugs in Nixfmt and issues with the format is relevant in many cases.

You can also reach us on Matrix at `#nix-formatting:nixos.org`.

## Development

Enter a development shell with `nix-shell`, `nix develop` or automatically with [direnv](https://direnv.net/), after which you can:
- Build (& run): `cabal build` (`cabal run`)
- Build and ignore warnings: `cabal build --flag -werror`. This is useful when
  hacking locally, but code must be free of warnings before merging.
- Debug: `cabal repl`
- Format the codebase: `treefmt`
- [Set up](https://haskell-language-server.readthedocs.io/en/latest/configuration.html#configuring-your-editor) your LSP-editor to use `haskell-language-server`

To run (almost) all CI checks locally:
```
nix-build -A ci
```

## Contributing code

Most changes to the format are going to be implemented in `Pretty.hs`.
Sometimes, other areas of the code base need fixing (CLI, Parser, etc.) as well.
However, if you find yourself touching `Predoc.hs`, please open an issue first or contact us on Matrix.
That file contains the IR and the renderer, they are brittle and require expertise.

### Testing

There are no unit tests; all tests are end to end.
Tests are run with `./test/test.sh` when no changes are to be expected, and with `./test/test.sh --update-diff` otherwise.
The diff must be added to the commit that caused it.

Our test suite mostly contains characterization tests (a.k.a. snapshot tests, golden master tests, etc.).
They are located in `tests/diff`.
To add a new test, create a folder and write unformatted code in `in.nix`, then run `./test/test.sh --update-diff` to generate the corresponding `out.nix` and `out-pure.nix` (they differ in the `--strict` flag passed to Nixfmt).
Some tests contain representative real-world code, however new tests should focus on extensively covering as many edge cases as possible.

Some tests are regression tests, located in `test/correct` and `tests/invalid`.
The former will simply test that the output never changes, the latter tests parser failures on incorrect input.
Tests in `test/correct` should be as minimalistic as possible, as to not trip on unrelated changes.
However this is not always avoidable and they occasionally need manual adjustment.

When editing a test or adding a new one, try to put it into a separate commit as to not mix input changes with diff changes.
Retrospectively fixing this requires advanced git rebasing skills, and using a helper tool like [lazygit](https://github.com/jesseduffield/lazygit) is strongly recommended.

### Documenting changes

User-facing changes should be documented for inclusion in the changelog and release notes, either via [changesets](https://knope.tech/reference/concepts/change-file/) or [conventional commits](https://knope.tech/reference/concepts/conventional-commits/).

> [!IMPORTANT]
> The actual `CHANGELOG.md` file should not be edited manually, as it is managed by Knope.

To create a "change file" in `.changeset`, you can run:
```console
knope document-change
```
This will interactively create a change file, prompting for summary and severity.

Alternatively, manually write a markdown file in `.changeset`, declaring the [semver](https://knope.tech/reference/concepts/semantic-versioning/) severity of the change:
```markdown
---
default: minor
---

# Summary of the change

Optional longer description of the change.
```

In the frontmatter, you are declaring how this change affects packages managed by Knope, of which we only have one: "default".

## Debugging

Short strings can easily be tested with `cabal v2-run --verbose=0 nixfmt -- -w=80 < <(echo $'some code here')`.
Note the usage of `$''` [ANSI-C quoting](https://www.gnu.org/software/bash/manual/html_node/ANSI_002dC-Quoting.html#ANSI_002dC-Quoting), which makes it easier to control the line breaks of the input.
`-w=80` is the default width used for the tests, however sometimes setting it to something ridiculously small can help.

The `--ast` flag outputs a pretty print of the parser result, which is especially helpful when debugging parser bugs.

The `--ir` flag outputs a pretty print of the IR produced by `Pretty.hs`, to inspect it before it gets rendered.

## Architecture overview

The data flow of the formatting process is as follows: (Text) → \[Parser\] → (AST) → \[Pretty\] → (IR) → \[Render\] → (Text).

### AST

The AST is defined in `Types.hs` (entry point type `File`).
Conceptually it sits somewhere between an annotated AST and a CST: It still uses tokens as primitive, however does not retain enough information to faithfully reproduce the parser input (as would typically be the case for a proper CST).

Every token is annotated with adjacent comments if present, and the line in the source code of the token.

### Pretty

`pretty` transforms each AST element into its IR by recursively walking the tree.
This is where the actual formatting logic is implemented.
Effectively, this is a giant switch case on all possible AST values, sometimes taking specific combinations of AST nodes into account for special casing.

### IR and rendering

*If you are familiar with [Wadler/Leijen style pretty printers](https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf) as commonly used in the Haskell ecosystem, this is another instance of them.*

The IR is defined by the `Doc` and `DocE` types in `Predoc.hs`.
It is a tree structure where the leaves are either text or whitespace and the intermediate nodes group the child nodes together.

The rough idea is that "Spacing" in the IR can become either a space or a line break, and which one will only be decided at rendering time.
Typically, either all spacings within a group will become spaces or newlines, depending on whether or not the entire group would fit onto a single line.
We call this process "expanding" groups, and the rendering algorithm will try to expand groups from outside to the inside.
This frees the Pretty phase of having to think too much about whether or not a piece of code will fit onto the rest of the line.

`layout` in `Predoc.hs` is the entry point of the rendering process, it will do various pre-processing on the IR and then call into `layoutGreedy` which implements the actual algorithm.

## Releasing

Releases are managed using [Knope](https://knope.tech), a CLI tool for generating changelogs and bumping versions using semver, conventional commits, and `.changeset/*.md` files.

Whenever we have unreleased changes, there should be a release PR showing what the next release will look like.
To publish the release, just merge the PR and CI will handle the rest.

### Creating an LTS branch

`knope.toml` configures the branch releases will target in the `CreatePullRequest` workflow step.
CI automation is triggered when the pushed branch matches the `base` branch configured in Knope.

If we need to maintain LTS releases for a version that `master` has diverged from, we must first create an LTS branch:

#### Switch to the older release

```console
git switch --detach v1.2.3
```

#### Create an LTS branch

```console
git switch --create lts/v1.2.x
```

#### Update `knope.toml`

```diff
  [[workflows.steps]]
  type = "CreatePullRequest"
- base = "master"
+ base = "lts/v1.2.x"
```

```console
git add --patch knope.toml
git commit -m "chore(knope): set base branch to lts/v1.2.x"
```

#### Push the LTS branch

```console
git push --set-upstream upstream lts/v1.2.x
```

#### Usage

CI will now recognise this as a release branch, because the branch name matches the `base` configured in `knope.toml`'s `CreatePullRequest` step.
Changes merged into the LTS branch will cause a release PR to be created, and releases merged into the LTS branch will be automatically published.

### Manually preparing a release

If you wish to make a release that differs from the automated release PR, you can create your own release PR.

#### Create a release branch

Before starting, switch to a new branch:
```console
git switch --create my-custom-release
```

Alternatively, checkout your new branch in a separate worktree:
```console
git worktree add ../my-custom-release
```

#### Prepare the release

Use Knope to bump the version and write release notes to the changelog:
```console
knope prepare-release
```

> [!TIP]
> See `knope prepare-release --help` for more options, including pre-release labels and manually specified version numbers.

> [!TIP]
> If Knope can't identify any changes since the latest tag, `prepare-release` will do nothing.
>
> Usually that's what you want, but if you need to prepare an "empty" release you can create a bogus change file in `.changeset`:
> ```markdown
> ---
> default: invalid
> ---
> ```
>
> `knope prepare-release` will empty `.changeset` anyway, so this file will not be committed.

Optionally, at this point you can manually edit the changelog entry that Knope created.

#### Commit changes

`prepare-release` will have staged its changes, but if you've edited anything you may need to stage your changes (e.g. using `git add --patch`).

Commit the release using:
```bash
git commit -m "chore: release $(knope print-version)"
```

#### Open a Pull Request

Create a Pull Request manually, or using the GitHub CLI:
```console
gh pr create
```

### Publishing a release

We've discussed automatic release PRs and manually creating a release PR, but a release is only published after the release has been merged.

Releases merged into a release branch are automatically published by CI:
- `knope.toml` is checked, to see whether `CreatePullRequest`→`base` matches the current branch name.
- The version is checked, to see whether it has been published already.
- A static `nixfmt` binary is compiled.
- A release is published to GitHub Releases, with the static `nixfmt` binary attached.

If CI fails, re-running the release workflow will attempt to publish again.
Additionally, any push to a release branch that has an unpublished version will attempt to create a release, so followup fixes will also attempt to release if the initial release failed.
