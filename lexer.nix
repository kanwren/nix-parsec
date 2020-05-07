let
  p = import ./parsec.nix;
in rec {
  # Build a space-consuming parser out of:
  #   - a parser that consumes spaces
  #   - a parser that consumes line comments
  #   - a parser that consumes block comments
  # None of these can accept empty input
  #   :: Parser null -> Parser null -> Parser null -> Parser null
  space = sp: lc: bc: p.skipMany (p.choice [sp lc bc]);

  # Use a space-consuming parser to turn a parser into a lexeme parser
  #   :: Parser null -> Parser a -> Parser a
  lexeme = sc: parser: p.thenSkip parser sc;

  # Use a space-consuming parser to turn a parser into a symbol parser
  #   :: Parser null -> String -> Parser String
  symbol = sc: sym: lexeme sc (p.string sym)
}
