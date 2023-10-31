<!-- © 2023 piegames <git@piegames.de>
   -
   - SPDX-License-Identifier: MPL-2.0
   -->

# Maintainer documentation

## Making a new release

- Check the commit log if anything is missing from the change log.
- Check dependency versions in `nixfmt.cabal`, bump upper bounds if possible.
- Bump the version.
- Give it a git tag.
- Upload to hackage using `cabal sdist`. See https://hackage.haskell.org/upload for details.
