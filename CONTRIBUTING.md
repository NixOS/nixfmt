# Contribution Guidelines

We welcome issues and pull requests at https://github.com/NixOS/nixfmt.
PRs that change the format should preferably be discussed in an issue first.

When contributing, please try to familiarize yourself with the [Nix Format Standard](https://github.com/NixOS/nixfmt/blob/master/standard.md), because the distinction between bugs in Nixfmt and issues with the format is relevant in many cases.

You can also reach us on Matrix at `#nix-formatting:nixos.org`.

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
