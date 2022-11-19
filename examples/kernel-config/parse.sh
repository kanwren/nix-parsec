#! /bin/sh

cd "$(dirname "$0")"

# TODO better than lib.traceValSeq
nix-instantiate --expr --strict '
  with import <nixpkgs> {};
  with import ./default.nix {};
  lib.traceValSeq
  (
    parseConfigFile ./surface-4.19.config
  )
'
