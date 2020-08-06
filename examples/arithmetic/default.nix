# Example: parse a simple arithmetic expression using "+" or "*" into its result
#
# This could backtrack a lot, though obviously it can be written to be much more
# efficient.
#
# Load in nix repl and test, e.g.:
#
# nix-repl> parseExpr "1 + 2 * -3"
# { type = "success"; value = -5; }

let
  nix-parsec = import ../../default.nix;
  inherit (nix-parsec) lexer;
in

with nix-parsec.parsec;

let
  spaces = skipWhile (c: c == " ");
  lexeme = lexer.lexeme spaces;
  symbol = lexer.symbol spaces;

  int = lexeme (lexer.signed spaces lexer.decimal);

  # Grammar:
  #   expr   ::= term + expr | term - expr | term
  #   term   ::= factor * term | factor / term | factor
  #   factor ::= (expr) | int

  expr = alt (bind term (n: skipThen (symbol "+") (fmap (m: n + m) expr))) term;
  term = alt (bind factor (n: skipThen (symbol "*") (fmap (m: n * m) term))) factor;
  factor = alt (between (symbol "(") (symbol ")") expr) int;
in {
  parseExpr = runParser (thenSkip expr eof);
}
