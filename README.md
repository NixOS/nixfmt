# `nix-format`

Tool to enforce a uniform style across Nix codebases, driven by [hnix][].

The fatal flaw of this implementation is that hnix doesn't keep newline and
comment information, so it is lost on pretty-print.

Additionally, getting output style up to par with what humans produce will
require a lot of work which has not been started yet.

If you want to help, begin with implementing [hnix#57][].

[hnix]: https://github.com/haskell-nix/hnix
[hnix#57]: https://github.com/haskell-nix/hnix/issues/57
