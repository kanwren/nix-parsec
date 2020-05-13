with (import ./parsec.nix);

let
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
  space = sp: lc: bc: skipMany (alt sp (alt lc bc));

  # Given the delimiter marking the start of a line comment, build a parser that
  # consume a line comment
  #   :: String -> Parser null
  skipLineComment = start:
    let
      prefix = string start;
    in skipThen prefix (skipWhile (x: x != "\n"));

  # Given start and end delimiters of a block comment, build a parser that
  # consumes a block comment
  #   :: String -> String -> Parser null
  skipBlockComment = start: end:
    let
      prefix = string start;
      suffix = string end;
    in skipThen prefix (skipTill anyChar suffix);

  # Use a space-consuming parser to turn a parser into a lexeme parser
  #   :: Parser null -> Parser a -> Parser a
  lexeme = sc: parser: thenSkip parser sc;

  # Use a space-consuming parser to turn a parser into a symbol parser
  #   :: Parser null -> String -> Parser String
  symbol = sc: sym: lexeme sc (string sym);

  # Parses a decimal integer.
  #
  # NOTE: since Nix can only handle 64-bit signed integers, the behavior on
  # larger numbers is not guaranteed.
  decimal =
    let
      toInt = builtins.fromJSON; # Hacky, but efficient
      int = fmap toInt (matchingN 19 "[[:digit:]]+");
      leadingZeros = skipWhile (c: c == "0");
    in alt
      # Nonzero number with leading zeros
      (skipThen leadingZeros int)
      # Only zeros
      (fmap (_: 0) (skipWhile1 (c: c == "0")));

  # Given a way to consume the space after the sign, and given a parser that
  # parses a number, return a parser that can also handle a leading +/- sign.
  #   :: Num a => Parser () -> Parser a -> Parser a
  signed = sp: parser:
    let
      plus = fmap (_: 1) (string "+");
      minus = fmap (_: -1) (string "-");
      sign = option 1 (alt minus plus);
    in bind sign (res: fmap (n: res * n) parser);

  # Parses a Nix character literal, without quotes. Handles character escaping.
  #
  # NOTE: Only supports \n, \r, and \t. All other characters after a backslash
  # will be returned as-is; e.g., "\a" becomes "a".
  charLit =
    bind anyChar
    (c: if c == "\\"
      then fmap escapeToChar anyChar
      else pure c);

  # Parses a basic double-quoted string literal, handling escaped inner quotes.
  stringLit = fmap (builtins.concatStringsSep "")
    (skipThen (string "\"") (manyTill charLit (string "\"")));
}
