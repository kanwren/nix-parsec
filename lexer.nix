let
  p = import ./parsec.nix;

  escapeToChar = c:
    if c == "n" then "\n"
    else if c == "r" then "\r"
    else if c == "t" then "\t"
    else c;

in rec {
  # Build a space-consuming parser out of:
  #   - a parser that consumes spaces
  #   - a parser that consumes line comments
  #   - a parser that consumes block comments
  # None of these should be able to accept empty input
  #   :: Parser null -> Parser null -> Parser null -> Parser null
  space = sp: lc: bc: p.skipMany (p.alt sp (p.alt lc bc));

  # Use a space-consuming parser to turn a parser into a lexeme parser
  #   :: Parser null -> Parser a -> Parser a
  lexeme = sc: parser: p.thenSkip parser sc;

  # Use a space-consuming parser to turn a parser into a symbol parser
  #   :: Parser null -> String -> Parser String
  symbol = sc: sym: lexeme sc (p.string sym);

  # Parses a decimal integer.
  #
  # NOTE: since Nix can only handle 64-bit signed integers, the behavior on
  # larger numbers is not guaranteed.
  decimal =
    let
      toInt = builtins.fromJSON; # Hacky, but efficient
      int = p.fmap toInt (p.matchingN 19 "[[:digit:]]+");
      leadingZeros = p.skipWhile (c: c == "0");
    in p.alt
      # Nonzero number with leading zeros
      (p.skipThen leadingZeros int)
      # Only zeros
      (p.fmap (_: 0) (p.skipWhile1 (c: c == "0")));

  # Given a way to consume the space after the sign, and given a parser that
  # parses a number, return a parser that can also handle a leading +/- sign.
  #   :: Num a => Parser () -> Parser a -> Parser a
  signed = sp: parser:
    let
      plus = p.fmap (_: 1) (p.string "+");
      minus = p.fmap (_: -1) (p.string "-");
      sign = p.option 1 (p.alt minus plus);
    in p.bind sign (res: p.fmap (n: res * n) parser);

  # Parses a Nix character literal, without quotes. Handles character escaping.
  #
  # NOTE: Only supports \n, \r, and \t. All other characters after a backslash
  # will be returned as-is; e.g., "\a" becomes "a".
  charLit =
    p.bind p.anyChar
    (c: if c == "\\"
      then p.fmap escapeToChar p.anyChar
      else p.pure c);

  # Parses a basic double-quoted string literal, handling escaped inner quotes.
  stringLit = p.fmap (builtins.concatStringsSep "")
    (p.skipThen (p.string "\"") (p.manyTill charLit (p.string "\"")));
}
