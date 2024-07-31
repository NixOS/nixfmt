#!/usr/bin/env bash

set -euo pipefail

# Simple test runner for nixfmt.
# Dependencies are declared in the dev shell (../flake.nix)

# Self-verify this script with shellcheck
shellcheck "$0"

# cd into the project root folder (cabal doesn't like it otherwise :/ )
cd "$(dirname "$0")/.."

shopt -s expand_aliases
if command -v cabal &> /dev/null; then
  # If we have cabal we're probably developing
  alias nixfmt="cabal v2-run --verbose=0 nixfmt -- -w 80"
else
  # Otherwise assume we're in CI, where instead nixfmt will be prebuilt
  alias nixfmt="nixfmt -w 80"
fi

# Do a test run to make sure it compiles fine
nixfmt --version

# Verify "correct", files that don't change when formatted
for file in test/correct/*.nix; do
  echo "Checking $file …"
  if ! out=$(nixfmt --pure --verify < "$file"); then
    echo "[ERROR] failed nixfmt verification"
    exit 1
  fi

  if diff --color=always --unified "$file" <(echo "$out"); then
    echo "[OK]"
  elif [[ $* == *--update-diff* ]]; then
    echo "$out" > "$file"
    echo "[UPDATED] $file"
  else
    echo "[ERROR] Formatting not stable (run with --update-diff to update the diff)"
    exit 1
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
  echo "Checking $file …"
  out="$(nixfmt --verify < "$file")"
  outfile="$(dirname "$file")/out.nix"

  if diff --color=always --unified "$outfile" <(echo "$out"); then
    echo "[OK]"
  elif [[ $* == *--update-diff* ]]; then
    echo "$out" > "$outfile"
    echo "[UPDATED] $outfile"
  else
    echo "[ERROR] (run with --update-diff to update the diff)"
    exit 1
  fi

  echo "Checking $file with --pure …"
  out="$(nixfmt --pure --verify < "$file")"
  outfile="$(dirname "$file")/out-pure.nix"

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
