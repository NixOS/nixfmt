# Maintainer documentation

Writing to .version in the root of the project causes this version to be returned by `nixfmt --version`
rather than the version from the cabalfile.

## Making a new release

- Bump the version in the [cabal file](./nixfmt.cabal)
- Update the [changelog](./CHANGELOG.md) with the new version
- Create a PR with the above changes and merge it
- After a successful build, a [new GitHub release](https://github.com/NixOS/nixfmt/releases) will automatically be created, along with a tag matching the version, via a GitHub Actions workflow. Keep an eye on the process to ensure everything runs smoothly!
