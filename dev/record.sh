#!/usr/bin/env bash
# Records a user's stars for the mock API, through the authenticated gh CLI so the
# unauthenticated budget the app uses isn't touched: dev/record.sh <login>
set -euo pipefail
login="${1:?usage: dev/record.sh <login>}"
dir="$(dirname "$0")/fixtures/$login"
mkdir -p "$dir"
gh api "users/$login" > "$dir/user.json"
page=1
while :; do
  gh api -H "Accept: application/vnd.github.star+json" "users/$login/starred?per_page=100&page=$page" > "$dir/page-$page.json"
  [ "$(jq length "$dir/page-$page.json")" -lt 100 ] && break
  page=$((page + 1))
done
echo "Recorded $page pages into $dir"
