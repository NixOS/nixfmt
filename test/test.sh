#!/bin/env bash
set -euo pipefail

# Simple test runner for nixfmt.
# Dependencies are declared in the dev shell (../flake.nix)

# Self-verify this script with shellcheck
shellcheck "$0"

# cd into the project root folder (cabal doesn't like it otherwise :/ )
cd "$(dirname "$0")/.."

shopt -s expand_aliases
alias nixfmt="cabal v2-run --verbose=0 nixfmt -- -w 80"

# Do a test run to make sure it compiles fine
nixfmt --version

# Verify "correct"
for file in test/correct/*.nix; do
  if ! nixfmt --verify < "$file" > /dev/null; then
    echo "[ERROR] $file failed nixfmt verification"
    exit 1
  else
    echo "[OK] $file"
  fi
done

# Verify "invalid"
for file in test/invalid/*.nix; do
  if nixfmt < "$file" > /dev/null 2>&1; then
    echo "[ERROR] $file should have failed nixfmt"
    exit 1
  else
    echo "[OK] $file"
  fi
done

# Verify "diff"
for file in test/diff/**/in.nix; do
  outfile="$(dirname "$file")/out.nix"

  echo "Checking $file …"
  out="$(nixfmt --verify < "$file")"

  if diff --color=always --unified "$outfile" <(echo "$out"); then
    echo "[OK]"
  elif [[ $* == *--update-diff* ]]; then
    echo "$out" > "$outfile"
    echo "[UPDATED] $outfile"
  else
    echo "[ERROR] (run with --update-diff to update the diff)"
    exit 1
  fi
done
