#! /bin/sh

cd "$(dirname "$0")"

# TODO better than lib.traceValSeq
nix-instantiate --expr --strict '
  with import <nixpkgs> {};
  #with import ./default.nix {}; error: value is a function while a set was expected
  #with import ./default.nix;
  with import ./from-xml.nix;
  lib.traceValSeq
  (
    #fromXml "<doc><val>hello</val></doc>"
    #fromXml "<doc/>"
    test
  )
'
