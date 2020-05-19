# Example: find the deepest depth of nested parentheses in a string of balanced
# parentheses, or return an error if the parentheses are not balanced.
#
# Load in nix repl and test, e.g.:
#
# nix-repl> parseParens "((())())"
# 3

let
  nix-parsec = import ../../default.nix;
in

with nix-parsec.parsec;

let
  max = x: y: if x > y then x else y;
  maximum = builtins.foldl' max 0;

  parens =
    let expr = fmap (x: x + 1) (between (string "(") (string ")") parens);
    in fmap maximum (many expr);
in {
  parseParens = runParser (thenSkip parens eof);
}
