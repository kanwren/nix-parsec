# Example: parse a UUID using nix-parsec and using regular expressions
#
# Load in nix repl and test, e.g.:
#
# nix-repl> :p parseUuid "123e4567-e89b-12d3-a456-426614174000"
# [ [ "123e4567" "e89b" "12d3" "a456" "426614174000" ] ]

{ pkgs ? import <nixpkgs> {}
}:

let
  inherit (pkgs) lib;
  nix-parsec = import ../../default.nix;
in

with nix-parsec.parsec;

let
  # Parses a UUID in a format like 123e4567-e89b-12d3-a456-426614174000
  uuid =
    let
      hex = satisfy (x: builtins.match "[0-9a-f]" x != null);
      nHex = n: fmap lib.concatStrings (count n hex);
      hyphen = string "-";
      group = n: next: skipThen hyphen (bind (nHex n) next);
    in
      bind (nHex 8)
      (g1: group 4
      (g2: group 4
      (g3: group 4
      (g4: group 12
      (g5: pure [g1 g2 g3 g4 g5])))));
in rec {
  # Parse using nix-parsec
  parseUuid = runParser uuid;

  # Parse using regular expressions
  parseUuid' = builtins.match "([0-9a-f]{8})-([0-9a-f]{4})-([0-9a-f]{4})-([0-9a-f]{4})-([0-9a-f]{12})";
}
