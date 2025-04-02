#!/usr/bin/env bash

set -euo pipefail

# https://stackoverflow.com/a/246128/6605742
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

# Self-verify this script with shellcheck
shellcheck "$0"

# Allows using a local directory for temporary files,
# which can then be inspected after the run
if (( $# > 0 )); then
    tmp=$(realpath "$1/tmp")
    if [[ -e "$tmp" ]]; then
        rm -rf "$tmp"
    fi
    mkdir -p "$tmp"
else
    tmp=$(mktemp -d)
    trap 'rm -rf "$tmp"' exit
fi

shopt -s expand_aliases
if command -v cabal &> /dev/null; then
  cd "$SCRIPT_DIR/.."
  cabal build exe:nixfmt
  nixfmtDir=$(dirname "$(cabal list-bin nixfmt)")
  export PATH=$nixfmtDir:$PATH
  # Otherwise assume we're in CI, where instead nixfmt will be prebuilt
fi

setup() {
  local name=$1

  git init --quiet "$tmp/$name"
  echo -e "\e[33mTesting $name in $tmp/$name\e[0m"
  cd "$tmp/$name" || exit 1

  git branch -m main

  # shellcheck disable=SC2016
  git config mergetool.nixfmt.cmd 'nixfmt --mergetool "$BASE" "$LOCAL" "$REMOTE" "$MERGED"'
  git config mergetool.nixfmt.trustExitCode true
}

# Successfully merges formatting-related merge conflicts
setup "success"

# Poorly-formatted file
cat > a.nix <<EOF
{  x
 , y
  ?[]
,z
}:
null
EOF

git add -A
git commit -q -m "init"

git branch -c feature

# Format file on main
nixfmt a.nix
git commit -a -q -m "format"

git switch -q feature

# Change file on feature branch
sed 's/y/why/' -i a.nix
git commit -a -q -m "change"

git switch -q main

# Try to merge the feature branch, will give a merge conflict
git merge feature >/dev/null && exit 1

# Resolve it automatically (should work because it's only related to formatting)
git mergetool -t nixfmt .
git commit -q --no-edit

echo -e "\e[32mSuccess!\e[0m"

# Check that files are merged correctly
setup "correct-merging"

# Poorly-formatted file
cat > a.nix <<EOF
{
  x,

  y,

  z
}:
null
EOF

git add -A
git commit -q -m "init"

git branch -c feature

# One normal change and one that will conflict with the reformat
sed 's/x/x2/;s/z/z2/' -i a.nix
git commit -a -q -m "change"

git switch -q feature
# One normal change and the reformat
sed 's/y/y2/' -i a.nix
nixfmt a.nix

git commit -a -q -m "change"

git switch -q main

# Try to merge the feature branch, will give a merge conflict
git merge feature >/dev/null && exit 1

# Resolve it automatically
git mergetool -t nixfmt .
if ! diff a.nix <(cat <<EOF
{
  x2,

  y2,

  z2,
}:
null
EOF
); then
  echo "Mergetool didn't call git merge-file correctly"
  exit 1
fi
git commit -q --no-edit

echo -e "\e[32mSuccess!\e[0m"

# Test that it doesn't try to resolve non-Nix files
setup "non-Nix"

# Non-Nix file
cat > a.md <<EOF
Hello there!
EOF
git add -A
git commit -q -m "init"

git branch -c feature

# Format file on main
sed 's/Hello/Hi/' -i a.md
git commit -a -q -m "change"

git switch -q feature

# Change file on feature branch
sed 's/Hello/Howdy/' -i a.md
git commit -a -q -m "change"

git switch -q main

# Try to merge the feature branch, will give a merge conflict
git merge feature >/dev/null && exit 1

# Resolve it automatically, should fail
git mergetool -t nixfmt . && exit 1

echo -e "\e[32mSuccessfully failed!\e[0m"


# Fails if any file is not valid Nix, error is merged
setup "invalid"

# Invalid Nix files
cat > a.nix <<EOF
{
  not = "valid";
EOF

git add -A
git commit -q -m "init"

git branch -c feature

# Format file on main
sed 's/valid/right/' -i a.nix
git commit -a -q -m "change"

git switch -q feature

# Change file on feature branch
sed 's/valid/correct/' -i a.nix
git commit -a -q -m "change"

git switch -q main

# Try to merge the feature branch, will give a merge conflict
git merge feature >/dev/null && exit 1

# Resolve it automatically (should work because it's only related to formatting)
git mergetool -t nixfmt . && exit 1

echo -e "\e[32mSuccessfully failed!\e[0m"


# Fails if there's an non-formatting related merge conflict
setup "non-formatting"

# Poorly-formatted file
cat > a.nix <<EOF
{  x
 , y
  ?[]
,z
}:
null
EOF

git add -A
git commit -q -m "init"

git branch -c feature

# Format file on main and change it
nixfmt a.nix
sed 's/y/where/' -i a.nix
git commit -a -q -m "format"

git switch -q feature

# Change file on feature branch in a conflicting way
sed 's/y/why/' -i a.nix
git commit -a -q -m "change"

git switch -q main

# Try to merge the feature branch, will give a merge conflict
git merge feature >/dev/null && exit 1

# Resolve it automatically, shouldn't work
git mergetool -t nixfmt . && exit 1

echo -e "\e[32mSuccessfully failed!\e[0m"
