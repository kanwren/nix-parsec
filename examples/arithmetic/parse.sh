#! /bin/sh

cd "$(dirname "$0")"

# TODO better than lib.traceValSeq
nix-instantiate --expr --strict '
  with import <nixpkgs> {};
  #with import ./default.nix {}; error: value is a function while a set was expected
  with import ./default.nix;
  lib.traceValSeq
  (
    parseExpr "1 + 2 * -3"
  )
'
