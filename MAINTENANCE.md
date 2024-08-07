# Maintainer documentation

Writing to .version in the root of the project causes this version to be returned by `nixfmt --version`
rather than the version from the cabalfile.

## Making a new release

- Check the commit log if anything is missing from the change log.
- Check dependency versions in `nixfmt.cabal`, bump upper bounds if possible.
- Bump the version.
- Give it a git tag.
- Upload to hackage using `cabal sdist`. See https://hackage.haskell.org/upload for details.
