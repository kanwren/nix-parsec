with (import ./parsec.nix);

let
  escapeToChar = c:
    if c == "n" then "\n"
    else if c == "r" then "\r"
    else if c == "t" then "\t"
    else c;

  hexDigits = {
    "0" = 0; "1" = 1; "2" = 2; "3" = 3; "4" = 4; "5" = 5; "6" = 6; "7" = 7;
    "8" = 8; "9" = 9;
    "A" = 10; "B" = 11; "C" = 12; "D" = 13; "E" = 14; "F" = 15;
    "a" = 10; "b" = 11; "c" = 12; "d" = 13; "e" = 14; "f" = 15;
  };

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

  # Given start and end delimiters of a block comment, build a parser that
  # consumes a possibly nested block comment
  #   :: String -> String -> Parser null
  skipBlockCommentNested = start: end:
    let
      prefix = string start;
      suffix = string end;
      go = skipThen prefix (skipTill (alt go anyChar) suffix);
    in go;

  # Use a space-consuming parser to turn a parser into a lexeme parser
  #   :: Parser null -> Parser a -> Parser a
  lexeme = sc: parser: thenSkip parser sc;

  # Use a space-consuming parser to turn a parser into a symbol parser
  #   :: Parser null -> String -> Parser String
  symbol = sc: sym: lexeme sc (string sym);

  # Parses a decimal integer. If you want to handle leading signs, wrap it using
  # 'signed'.
  #
  # Unlike 'decimal', it is often faster, but will
  # only ever consume at most 19 characters after leading zeros. Only use this
  # if you're sure the number you're parsing can fit in a signed 64-bit integer.
  unsafeDecimal =
    let
      toInt = builtins.fromJSON; # Hacky, but efficient
      int = fmap (x: toInt (builtins.elemAt x 0)) (matchingN 19 "[[:digit:]]+");
      leadingZeros = skipWhile (c: c == "0");
    in alt
      # Nonzero number with leading zeros
      (skipThen leadingZeros int)
      # Only zeros
      (fmap (_: 0) (skipWhile1 (c: c == "0")));

  # Parses a decimal integer. If you want to handle leading signs, wrap it using
  # 'signed'.
  #
  # NOTE: since Nix can only handle 64-bit signed integers, the behavior on
  # larger numbers is not guaranteed.
  decimal =
    let isDigit = c: builtins.match "[[:digit:]]" c != null;
    in fmap builtins.fromJSON (takeWhile1 isDigit);

  # Parses a binary integer, as a nonempty string of "0"s and "1"s. Does not
  # assume a prefix.
  binary =
    let
      isBinDigit = c: c == "0" || c == "1";
      binToInt = str:
        let
          len = builtins.stringLength str;
          nthDigit = n: if builtins.substring n 1 str == "0" then 0 else 1;
          go = acc: i:
            if i >= len
              then acc
              else go (2 * acc + nthDigit i) (i + 1);
        in go 0 0;
    in fmap binToInt (takeWhile1 isBinDigit);

  # Parses a hexadecimal integer, as a nonempty string of digits "0" through "9"
  # or letters "A" through "F" (either uppercase or lowercase). Does not assume
  # a prefix.
  hexadecimal =
    let
      isHexDigit = c: builtins.match "[[:xdigit:]]" c != null;
      hexToInt = str:
        let
          len = builtins.stringLength str;
          nthDigit = n: hexDigits.${builtins.substring n 1 str};
          go = acc: i:
            if i >= len
              then acc
              else go (16 * acc + nthDigit i) (i + 1);
        in go 0 0;
    in fmap hexToInt (takeWhile1 isHexDigit);

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
