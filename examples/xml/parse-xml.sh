#! /bin/sh

cd "$(dirname "$0")"

input="$1"
if [ -z "$input" ]
then
  echo "usage:" >&2
  echo "$0 '<doc>text</doc>'" >&2
  exit 1
fi

inputQuoted="$(echo '""' | jq '$s' --arg s "$input")"

outputJson="$(
  nix-instantiate --expr --json --eval --strict --arg input "$inputQuoted" '
    { input }:
    with import ./parse-xml.nix;
    printYaml (parseXml input).value
  '
)"

echo "$outputJson" | jq -r
