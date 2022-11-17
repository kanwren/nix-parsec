#! /bin/sh

cd "$(dirname "$0")"

# TODO better than lib.traceValSeq
nix-instantiate --expr --strict '
  with import <nixpkgs> {};
  with import ./default.nix {};
  lib.traceValSeq
  (
    parseUuid "123e4567-e89b-12d3-a456-426614174000"
  )
'
