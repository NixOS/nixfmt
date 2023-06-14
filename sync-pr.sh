#!/usr/bin/env bash

set -euo pipefail

#tmp=$(mktemp -d)
tmp=/tmp/nixfmt
mkdir -p "$tmp"
cd "$tmp"
rm -rf *
echo "Working in directory $tmp"
# trap 'echo -n "enter when not needed anymore: $tmp"; read; rm -rf "$tmp"' exit

bodyForCommit() {
  local index=$1
  local commit=$2
  if (( index == 0 )); then
    subject=$(git -C nixfmt show -s --format=%s "$commit")
    url="$nixfmtUrl"/commit/"$commit"
    echo -e "base: $subject\n\nFormat using the base commit from nixfmt PR $nixfmtPrNumber: $url"
  else
    commit=$(git -C nixfmt show -s --pretty=%s "$commit")
    subject=$(git -C nixfmt show -s --format=%s "$commit")
    url="$nixfmtUrl"/pull/"$nixfmtPrNumber"/commits/"$commit"
    echo -e "$index: $subject\n\nFormat using commit number $index from nixfmt PR $nixfmtPrNumber: $url"
  fi
}

step() {
  echo -e "\e[34m$1\e[0m"
}

isLinear() {
  local repo=$1
  local revs=$2
  for mergeCommit in $(git 2>/dev/null -C "$repo" log --pretty=format:%H --min-parents=2 "$revs"); do
    return 1
  done
}


set -x
nixfmtUrl=$1
nixfmtPrNumber=$2
nixpkgsMirrorUrl=$3
set +x

nixpkgsUpstreamUrl=https://github.com/NixOS/nixpkgs
nixpkgsMirrorBranch=nixfmt-$nixfmtPrNumber

step "Fetching nixfmt pull request and creating a branch for the head commit"
git init nixfmt
git -C nixfmt fetch "$nixfmtUrl" "refs/pull/$nixfmtPrNumber/merge"
nixfmtBaseCommit=$(git -C nixfmt rev-parse FETCH_HEAD^1)
nixfmtHeadCommit=$(git -C nixfmt rev-parse FETCH_HEAD^2)
git -C nixfmt switch -c main "$nixfmtHeadCommit"

step "Linearising nixfmt history after the base commit"
# https://stackoverflow.com/a/17994534
FILTER_BRANCH_SQUELCH_WARNING=1 git -C nixfmt filter-branch --parent-filter 'cut -f 2,3 -d " "' --msg-filter 'echo $GIT_COMMIT' "$nixfmtBaseCommit"..main

nixfmtCommitCount=$(git -C nixfmt rev-list --count "$nixfmtBaseCommit"..main)
if (( nixfmtCommitCount == 0 )); then
  step "No commits, deleting the nixpkgs branch $nixpkgsPushBranch if it exists"
  # git push requires a repository to work at all, _any_ repository
  git init -q trash
  git -C trash push "$nixpkgsPushRepository" :refs/heads/"$nixpkgsPushBranch"
  rm -rf trash
  exit 0
else
  echo "There are $nixfmtCommitCount linearised commits"
fi

commitsToMirror=("$nixfmtBaseCommit")
readarray -t -O 1 commitsToMirror < <(git -C nixfmt rev-list --reverse "$nixfmtBaseCommit"..main)

step "Fetching upstream Nixpkgs commit history"
git init --bare nixpkgs.git

git -C nixpkgs.git remote add upstream "$nixpkgsUpstreamUrl"
git -C nixpkgs.git config remote.upstream.promisor true
git -C nixpkgs.git config remote.upstream.partialclonefilter tree:0

git -C nixpkgs.git fetch --no-tags upstream HEAD:master

step "Finding the last Nixpkgs commit before the first commit on nixfmt's branch"
nixfmtFirstCommit=${commitsToMirror[1]}
# Commit date, not author date, not sure what's better
nixfmtFirstCommitDateEpoch=$(git -C nixfmt log -1 --format=%ct "$nixfmtFirstCommit")
nixfmtFirstCommitDateHuman=$(git -C nixfmt log -1 --format=%ci "$nixfmtFirstCommit")
echo "The first nixfmt commit is $nixfmtFirstCommit on $nixfmtFirstCommitDateHuman"

nixpkgsBaseCommit=$(git -C nixpkgs.git rev-list -1 master --before="$nixfmtFirstCommitDateEpoch")
nixpkgsBaseCommitDateHuman=$(git -C nixpkgs.git log -1 --format=%ci "$nixpkgsBaseCommit")

echo "The last Nixpkgs commit before then is $nixpkgsBaseCommit on $nixpkgsBaseCommitDateHuman, which will be used as the Nixpkgs base commit"

step "Fetching mirror Nixpkgs commit history in branch $nixpkgsMirrorBranch if it exists"
git -C nixpkgs.git remote add mirror "$nixpkgsMirrorUrl"
git -C nixpkgs.git config remote.mirror.promisor true
git -C nixpkgs.git config remote.mirror.partialclonefilter tree:0

# After this:
# - $startingCommit should be the nixpkgs commit that the branch should be reset to
# - $extraCommits should be the number of commits already processed
if ! git -C nixpkgs.git fetch --no-tags mirror "$nixpkgsMirrorBranch":mirrorBranch; then
  echo "There is not"
  startingCommit=$nixpkgsBaseCommit
  extraCommits=0
else
  echo "There is, it points to $(git -C nixpkgs.git rev-parse mirrorBranch)"
  step "Checking to which extent work from the existing branch can be reused"
  if [[ -z "$(git -C nixpkgs.git branch --contains="$nixpkgsBaseCommit" mirrorBranch)" ]]; then
    echo "It is not"
    startingCommit=$nixpkgsBaseCommit
    extraCommits=0
  else
    echo "It is!"
    echo "Checking if the branch has a linear history"
    if ! isLinear nixpkgs.git "$nixpkgsBaseCommit"..mirrorBranch; then
      echo "It is not linear, resetting the branch"
      startingCommit=$nixpkgsBaseCommit
      extraCommits=0
    else
      echo "It is linear!"
      nixpkgsCount=$(git -C nixpkgs.git rev-list --count "$nixpkgsBaseCommit"..mirrorBranch)
      echo "There's $nixpkgsCount commits in the branch on top of the base commit"
      extraCommits=0
      # Check if there's at least 1 commits in nixpkgs and at least 0 commits in nixfmt
      # Check if commit 1 in nixpkgs corresponds to commit 0 in nixfmt
      # If true, increase extraCommits by one, otherwise break
      # Check if there's at least 2 commits in nixpkgs and at least 1 commits in nixfmt
      # If so, check if commit 2 in nixpkgs corresponds to commit 1 in nixfmt
      # ...
      while
        if (( nixpkgsCount >= extraCommits + 1 && nixfmtCommitCount >= extraCommits)); then
          echo "Checking whether commit with index $(( extraCommits + 1 )) in nixpkgs corresponds to commit with index $extraCommits in nixfmt"
          nixpkgsCommit=$(git -C nixpkgs.git rev-parse "mirrorBranch~$((nixpkgsCount - (extraCommits + 1)))")
          body=$(git -C nixpkgs.git log -1 "$nixpkgsCommit" --pretty=%B)
          nixfmtCommit=${commitsToMirror[$extraCommits]}
          expectedBody=$(bodyForCommit "$extraCommits" "$nixfmtCommit")
          if [[ "$body" == "$expectedBody" ]]; then
            echo "It does!"
          else
            echo "It does not, body of nixpkgs commit $nixpkgsCommit is"
            echo "$body"
            echo "But expected body is"
            echo "$expectedBody"
            false
          fi
        else
          false
        fi
      do
        extraCommits=$(( extraCommits + 1 ))
      done

      nixpkgsCommit=$(git -C nixpkgs.git rev-parse "mirrorBranch~$(( nixpkgsCount - extraCommits ))")
      startingCommit="$nixpkgsCommit"
    fi
  fi
fi

echo "Starting commit is $startingCommit, extraCommits is $extraCommits"

# Remove the history-only nixpkgs
rm -rf nixpkgs.git

set -x

step "Fetching contents of Nixpkgs base commit $nixpkgsBaseCommit"
git init nixpkgs
git -C nixpkgs fetch --no-tags --depth 1 "$nixpkgsUpstreamUrl" "$nixpkgsBaseCommit":base

step "Fetching contents of the starting commit and updating the mirror branch"
if (( extraCommits == 0 )); then
  git -C nixpkgs switch -c mirrorBranch "$startingCommit"
else
  git -C nixpkgs fetch --no-tags --depth 1 "$nixpkgsMirrorUrl" "$startingCommit":mirrorBranch
  git -C nixpkgs switch mirrorBranch
fi

git -C nixpkgs push --force "$nixpkgsMirrorUrl" mirrorBranch:"$nixpkgsMirrorBranch"

if (( extraCommits == 0 )); then
  index=0
else
  index=$(( extraCommits - 1 ))
fi

if (( index == nixfmtCommitCount )); then
  echo "Nothing to do"
  exit 0
fi

updateToIndex() {
  nixfmtCommit=${commitsToMirror[$index]}

  step "Checking out nixfmt at $nixfmtCommit"
  git -C nixfmt checkout -q "$nixfmtCommit"

  step "Building nixfmt"
  nix build ./nixfmt
}

applyNixfmt() {
  step "Checking out Nixpkgs at the base commit"
  git -C nixpkgs checkout "$nixpkgsBaseCommit" -- .

  step "Running nixfmt on nixpkgs"
  if ! xargs -r -0 -P"$(nproc)" -n1 -a <(find nixpkgs -type f -name '*.nix' -print0) result/bin/nixfmt; then
    echo -e "\e[31mFailed to run nixfmt on some files\e[0m"
  fi
}

commitResult() {
  step "Committing the formatted result"
  git -C nixpkgs add -A
  git -C nixpkgs commit --allow-empty -m "$(bodyForCommit "$index" "$nixfmtCommit")"

  step "Pushing result"
  git -C nixpkgs push "$nixpkgsMirrorUrl" mirrorBranch:"$nixpkgsMirrorBranch"
}


updateToIndex

appliedNixfmtPath=$(realpath result)

if (( extraCommits == 0 )); then
  applyNixfmt
  commitResult
fi

while (( index != nixfmtCommitCount )); do
  index=$(( index + 1 ))

  updateToIndex

  step "Formatting nixpkgs"
  if [[ "$appliedNixfmtPath" != "$(realpath result)" ]]; then
    applyNixfmt
    commitResult
    appliedNixfmtPath=$(realpath result)
  else
    echo "The nixfmt store path didn't change, saving ourselves a formatting"
    commitResult
  fi
done



# Depending on extraCommits:
# - 0 -> Build the base formatter, apply the base formatting and commit it, index = 0
# - 1 -> Build the base formatter, no need to apply it          index = 0
# - 2 -> Build the formatter at commit 1, no need to apply it   index = 1
# - 3 -> Build the formatter at commit 2, no need to apply it   index = 2
# - ...
# - $nixfmtCommitCount + 1 -> Build the formatter at commit $nixfmtCommitCount, no need to apply it, index = $nixfmtCommitCount
#   Actually, in this case we're done right away

# The invariant here should be:
# 1. [x] Nixpkgs branch checked out
# 2. [x] Remote branch matches local branch
# 3. The currently checked out nixfmt is applied in the latest commit
# 4. $index is the nixfmt commit that is checked out, may be 0, may be the number of total commits
# 5. $appliedNixfmtPath points to the store path of the nixfmt built from the current checkout

#echo "Branch points to $(git -C nixpkgs.git rev-parse "$nixpkgsPushBranch")"
#echo "extraCommits is $extraCommits"

#exit 0

# TODO: Is this even needed?
#step "Checking out $nixpkgsBaseCommit"
#git >/dev/null 2>/dev/null -C nixpkgs.git worktree add "$PWD"/nixpkgs "$nixpkgsBaseCommit"

# No formatting is applied yet, 0
# Formatting is already applied at higher levels, 1, 2, ...
#init() {
#  index=$1
#  nixfmtCommit=$(nixfmtCommitByIndex "$index")
#  step "Checking out nixfmt at $nixfmtCommit"
#  git -C nixfmt.git worktree >/dev/null 2>/dev/null add "$PWD"/nixfmt "$nixfmtBaseCommit"
#  index=0
#}
# git >/dev/null 2>/dev/null -C nixpkgs.git worktree add -b "$nixpkgsPushBranch" "$PWD"/nixpkgs "$nixpkgsBaseCommit"
# Ensures invariant 2.
#



#currentFormat=
## TODO: Currently this runs the formatter even if it would've been the same binary as a previous cached run
#
#while [[ "$index" -ge 0 ]]; do
#  nixfmtCommit=$(git -C nixfmt.git rev-parse "$nixfmtHeadCommit~$index")
#  echo "Checking out nixfmt at commit $nixfmtCommit ($index before head)"
#  git -C nixfmt checkout "$nixfmtCommit"
#  echo "Building nixfmt"
#  nix build ./nixfmt
#  if [[ "$currentFormat" != "$(realpath result)" ]]; then
#    echo "Running nixfmt on nixpkgs"
#    xargs -r -0 -P"$(nproc)" -n1 -a <(find nixpkgs -type f -name '*.nix' -print0) result/bin/nixfmt
#    currentFormat=$(realpath result)
#  else
#    echo "Skipping nixfmt run because the build didn't change"
#  fi
#  echo "Committing result"
#  git -C nixpkgs add -A
#  git -C nixpkgs commit --allow-empty -m "$(bodyForCommit "$nixfmtCommit")"
#
#  echo "Pushing result"
#  git -C nixpkgs push --force mirror "$nixpkgsPushBranch"
#
#  index=$((index - 1))
#done
#
