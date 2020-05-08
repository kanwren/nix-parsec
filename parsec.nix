# A parser is a value with the following type:
#   type Parser a = (String, Int, Int) -> Maybe (a, Int, Int)
#
# - The parameters are the source, the offset, and the length
# - The result is the value produced, the new offset, and the new length
# - If a failure occurs, the result will be 'null'

with builtins;

with rec {
  # Redefinitions to avoid depending on lib

  foldr = op: nul: list:
    let
      len = length list;
      fold' = n:
        if n == len
        then nul
        else op (elemAt list n) (fold' (n + 1));
    in fold' 0;

  # TODO: optimize result collection
  reverseList = xs:
    let l = length xs; in genList (n: elemAt xs (l - n - 1)) l;
};

rec {
  # running {{{

  # Run a parser, returning the result in a single-element list, or 'null' if it
  # failed. This is to disambiguate between failing and suceeding with 'null'.
  #
  # If the parser did not consume all of its input, this will still succeed. If
  # you want to make sure all input has been consume, use 'eof'.
  #
  #   :: Parser a -> String -> [a]
  runParser = parser: str:
    let res = parser [str 0 (stringLength str)];
    in if failed res then [] else [(elemAt res 0)];

  # Did a parser fail?
  #   :: Maybe (a, Int, Int) -> Bool
  failed = ps: ps == null;

  # }}}

  # queries {{{

  # Query the current state of the parser
  #   :: Parser (String, Int, Int)
  state = ps:
    let
      offset = elemAt res 1;
      len = elemAt res 2;
    in [ps offset len];

  # Augment a parser to also return the number of characters it consumed
  #   :: Parser a -> Parser (Int, a)
  measure = parser: ps:
    let
      initialOffset = elemAt ps 1;
      res = parser ps;
    in if failed res
      then null
      else let
        value = elemAt res 0;
        newOffset = elemAt res 1;
        newLength = elemAt res 2;
      in [[(newOffset - initialOffset) value] newOffset newLength];

  # Augment a parser to also return the characters it consumed
  #   :: Parser a -> Parser (String, a)
  withMatch = parser: ps:
    let
      str = elemAt ps 0;
      oldOffset = elemAt ps 1;
      res = parser ps;
    in if failed res
      then null
      else let
        value = elemAt res 0;
        newOffset = elemAt res 1;
        newLen = elemAt res 2;
      in [[(substring oldOffset (newOffset - oldOffset) str) value] newOffset newLen];

  # }}}

  # composition {{{

  # Map a function over the result of a parser
  #   :: (a -> b) -> Parser a -> Parser b
  fmap = f: parser: ps:
    let
      res = parser ps;
      val = elemAt res 0;
      offset = elemAt res 1;
      len = elemAt res 2;
    in if failed res
      then null
      else [(f val) offset len];

  # Lift a value into a parser
  #   :: a -> Parser a
  pure = x: ps: [x (elemAt ps 1) (elemAt ps 2)];

  # Monadic bind; sequence two parsers together
  #   :: Parser a -> (a -> Parser b) -> Parser b
  bind = parser: f: ps:
    let
      str = elemAt ps 0;
      res1 = parser ps;
    in if failed res1
      then null
      else let
        val = elemAt res1 0;
        offset = elemAt res1 1;
        len = elemAt res1 2;
      in (f val) [str offset len];

  # Sequence two parsers, ignoring the result of the first one
  #   :: Parser a -> Parser b -> Parser b
  skipThen = parser1: parser2: bind parser1 (_: parser2);

  # Sequence two parsers, ignoring the result of the second one
  #   :: Parser a -> Parser b -> Parser a
  thenSkip = parser1: parser2: bind parser1 (x: fmap (_: x) parser2);

  # Ignore the results of a parser
  #   :: Parser a -> Parser null
  void = fmap (_: null);

  # }}}

  # options and failure {{{

  # Parser that always fails (the identity under 'alt')
  #   :: Parser a
  fail = _: null;

  # Run two parsers; if the first one fails, run the second one
  #   :: Parser a -> Parser a -> Parser a
  alt = parser1: parser2: ps:
    let
      str = elemAt ps 0;
      res1 = parser1 ps;
      res2 = parser2 ps;
    in if failed res1 then res2 else res1;

  # Try to apply a parser, or return a default value if it fails without
  # consuming input
  #   :: a -> Parser a -> Parser a
  option = def: parser: alt parser (pure def);

  # Try to apply a parser. If it succeeds, return its result in a singleton
  # list, and if it fails without consuming input, return an empty list
  #   :: Parser a -> Parser [a]
  optional = parser: alt (fmap (x: [x]) parser) (pure []);

  # Run a list of parsers, using the first one that succeeds
  #   :: [Parser a] -> Parser a
  choice = foldr alt fail;

  # }}}

  # consumption primitives {{{

  # Consumes a character if it satisfies a predicate
  #   :: (Char -> Bool) -> Parser Char
  satisfy = pred: ps:
    let
      str = elemAt ps 0;
      offset = elemAt ps 1;
      len = elemAt ps 2;
      c = substring offset 1 str; # the next character
    in if len > 0 && pred c
      then [c (offset + 1) (len - 1)]
      else null;

  # Consumes a character if it satisfies a predicate, applying a function to the
  # result.
  #   :: (Char -> a) -> (Char -> Bool) -> Parser a
  satisfyWith = f: pred: ps:
    let
      str = elemAt ps 0;
      offset = elemAt ps 1;
      len = elemAt ps 2;
      c = substring offset 1 str; # the next character
    in if len > 0 && pred c
      then [(f c) (offset + 1) (len - 1)]
      else null;

  # Consume any character
  #   :: Parser Char
  anyChar = satisfy (_: true);

  # Consume any character except a given character
  #   :: Char -> Parser Char
  anyCharBut = c: satisfy (x: x != c);

  # Given a string, try to consume it from the input and return it if it
  # suceeds. If it fails, DON'T consume any input.
  #   :: String -> Parser String
  string = pr: ps:
    let
      prefixLen = stringLength pr;
      str = elemAt ps 0;
      offset = elemAt ps 1;
      len = elemAt ps 2;
    in if len >= prefixLen && substring offset prefixLen str == pr
      then [pr (offset + prefixLen) (len - prefixLen)]
      else null;

  # 'notFollowedBy p' only succeeds when 'p' fails, and never consumes any input
  #   :: Parser a -> Parser null
  notFollowedBy = parser: ps:
    let
      offset = elemAt ps 1;
      len = elemAt ps 2;
    in if failed (parser ps)
      then [null offset len]
      else null;

  # Fails if there is still more input remaining, returns null otherwise
  #   :: Parser null
  eof = ps:
    let
      offset = elemAt ps 1;
      len = elemAt ps 2;
    in if len == 0
      then [null offset len]
      else null;

  # Return whether or not we're at the end of the input.
  #   :: Parser Bool
  atEnd = ps:
    let
      offset = elemAt ps 1;
      len = elemAt ps 2;
    in [(len == 0) offset len];

  # }}}

  # takes {{{

  # Repeat a parser 'n' times, returning the results from each parse
  #   :: Int -> Parser a -> Parser [a]
  count = n: assert n >= 0; parser:
    let go = m: if m == 0
      then pure []
      else bind parser (first: fmap (rest: [first] ++ rest) (go (m - 1)));
    in go n;

  # Consume 'n' characters, or fail if there's not enough characters left.
  # Return the characters consumed.
  #   :: Int -> Parser String
  take = n: assert n >= 0; ps:
    let
      str = elemAt ps 0;
      offset = elemAt ps 1;
      len = elemAt ps 2;
    in if n <= len
      then [(substring offset n str) (offset + n) (len - n)]
      else null;

  # Consume zero or more characters while the predicate holds, returning the
  # consumed characters.
  #   :: (Char -> Bool) -> Parser String
  takeWhile = pred: ps:
    let
      str = elemAt ps 0;
      offset = elemAt ps 1;
      len = elemAt ps 2;
      strLen = stringLength str;
      # Search for the next offset that violates the predicate
      go = ix:
        if ix >= strLen || !pred (substring ix 1 str)
          then ix
          else go (ix + 1);
      endIx = go offset;
      # The number of characters we found
      numChars = endIx - offset;
    in [(substring offset numChars str) endIx (len - numChars)];

  # Consume one or more characters while the predicate holds, returning the
  # consumed characters.
  #   :: (Char -> Bool) -> Parser String
  takeWhile1 = pred:
    bind (satisfy pred) (first: fmap (rest: first + rest) (takeWhile pred));

  # Apply a parser zero or more times until it fails, returning a list of the
  # results
  #   :: Parser a -> Parser [a]
  many = parser:
    let go = alt (bind parser (first: fmap (rest: [first] ++ rest) go)) (pure []);
    in go;

  # Apply a parser one or more times until it fails, returning a list of the
  # resuls
  #   :: Parser a -> Parser (NonEmpty a)
  many1 = parser:
    bind parser (first: fmap (rest: [first] ++ rest) (many parser));

  # Repeat a parser zero or more times until the end parser succeeds. Returns a
  # list of the results of the first parser.
  #   :: Parser a -> Parser b -> Parser [a]
  manyTill = parser: end:
    let go = alt (fmap (_: []) end) (bind parser (first: fmap (rest: [first] ++ rest) go));
    in go;

  # Repeat a parser one or more times until the end parser succeeds. Returns a
  # list of the results of the first parser.
  #   :: Parser a -> Parser b -> Parser (NonEmpty a)
  manyTill1 = parser: end:
    bind parser (first: fmap (rest: [first] ++ rest) (manyTill parser end));

  # }}}

  # separators {{{

  # Sequence three parsers, 'before', 'after', and 'middle', running them in the
  # obvious order and keeping the middle result.
  # Example: parens = between (string "(") (string ")")
  #
  #   :: Parser a -> Parser b -> Parser c -> Parser c
  between = before: after: middle: skipThen before (thenSkip middle after);

  # Parse zero or more occurrances of the first parser, separated by the second
  # parser. Returns a list of results of the first parser.
  #   :: Parser a -> Parser b -> Parser [a]
  sepBy = parser: end:
    alt (sepBy1 parser end) (pure []);

  # Parse one or more occurrances of the first parser, separated by the second
  # parser. Returns a list of results of the first parser.
  #   :: Parser a -> Parser b -> Parser (NonEmpty a)
  sepBy1 = parser: end:
    bind parser (first: fmap (rest: [first] ++ rest) (many (skipThen end parser)));

  # Parse zero or more occurrances of the first parser, separated and ended by
  # the second parser. Returns a list of results of the first parser.
  #   :: Parser a -> Parser b -> Parser [a]
  endBy = parser: end:
    many (thenSkip parser end);

  # Parse one or more occurrances of the first parser, separated and ended by
  # the second parser. Returns a list of results of the first parser.
  #   :: Parser a -> Parser b -> Parser (NonEmpty a)
  endBy1 = parser: end:
    many1 (thenSkip parser end);

  # Parse zero or more occurrances of the first parser, separated and optionally
  # ended by the second parser. Returns a list of result of the first parser.
  #   :: Parser a -> Parser b -> Parser [a]
  sepEndBy = parser: end:
    alt (sepEndBy1 parser end) (pure []);

  # Parse one or more occurrances of the first parser, separated and optionally
  # ended by the second parser. Returns a list of result of the first parser.
  #   :: Parser a -> Parser b -> Parser (NonEmpty a)
  sepEndBy1 = parser: end:
    let
      go = alt
        (skipThen end (alt
          (bind parser (first: fmap (rest: [first] ++ rest) go))
          (pure [])))
        (pure []);
    in bind parser (first: fmap (rest: [first] ++ rest) go);

  # }}}

  # skips {{{

  # Consume 'n' characters, or fail if there's not enough characters left.
  #   :: Int -> Parser null
  skip = n: assert n >= 0; ps:
    let
      str = elemAt ps 0;
      offset = elemAt ps 1;
      len = elemAt ps 2;
    in if n <= len
      then [null (offset + n) (len - n)]
      else null;

  # Consume zero or more characters while the predicate holds
  #   :: (Char -> Bool) -> Parser null
  skipWhile = pred: ps:
    let
      str = elemAt ps 0;
      offset = elemAt ps 1;
      len = elemAt ps 2;
      # Search for the next offset that violates the predicate
      go = ix:
        if ix >= len || !pred (substring ix 1 str)
          then ix
          else go (ix + 1);
      endIx = go offset;
      # The number of characters we found
      numChars = endIx - offset;
    in [null endIx (len - numChars)];

  # Consume one or more characters while the predicate holds
  #   :: (Char -> Bool) -> Parser null
  skipWhile1 = pred:
    skipThen (satisfy pred) (skipWhile pred);

  # Run a parser zero or more times until it fails, discarding all the input
  # that it accepts.
  #   :: Parser a -> Parser null
  skipMany = parser:
    let go = alt (skipThen parser go) (pure null);
    in go;

  # Run a parser one or more times until it fails, discarding all the input that
  # it accepts.
  #   :: Parser a -> Parser null
  skipMany1 = parser: skipThen parser (skipMany parser);

  # Repeat a parser zero or more times until the end parser succeeds. Discards
  # consumed input.
  #   :: Parser a -> Parser b -> Parser null
  skipTill = parser: end:
    let go = alt end (skipThen parser go);
    in void go;

  # Repeat a parser one or more times until the end parser succeeds. Discards
  # consumed input.
  #   :: Parser a -> Parser b -> Parser null
  skipTill1 = parser: end:
    skipThen parser (skipTill parser end);

  # }}}

  # peeks and drops {{{

  # Examine the next character without consuming it. Fails if there's no input
  # left.
  #   :: Parser Char
  peek = ps:
    let
      str = elemAt ps 0;
      offset = elemAt ps 1;
      len = elemAt ps 2;
    in if len > 0
      then [(substring offset 1 str) offset len]
      else null;

  # Examine the rest of the input without consuming it
  #   :: Parser String
  peekRest = ps:
    let
      str = elemAt ps 0;
      offset = elemAt ps 1;
      len = elemAt ps 2;
    in [(substring offset len str) offset len];

  # Consume and return the rest of the input
  #   :: Parser String
  consumeRest = ps:
    let
      str = elemAt ps 0;
      offset = elemAt ps 1;
      len = elemAt ps 2;
    in [(substring offset len str) (offset + len) 0];

  # Consume and ignore the rest of the input
  #   :: Parser null
  dropRest = ps:
    let
      offset = elemAt ps 1;
      len = elemAt ps 2;
    in [null (offset + len) 0];

  # }}}

  # regex {{{

  # Given a regex that matches a string, consume characters matching that regex,
  # or fail if the next characters in the input do not match.
  #
  # NOTE: This has to copy the rest of the string, so if you know the maximum
  # number of characters you may need, use "matchingN".
  #
  #   :: String -> Parser String
  matching = regex: ps:
    let len = elemAt ps 2;
    in matchingN len regex ps;

  # Given a regex that matches a string, consume at most 'n' characters from the
  # input matching the regular expression. Return the matched text.
  #   :: Int -> String -> Parser String
  matchingN = n: assert n >= 0; regex: ps:
    let
      str = elemAt ps 0;
      offset = elemAt ps 1;
      len = elemAt ps 2;
      result = match ("(" + regex + ").*") (substring offset n str);
    in if result == null
      then null
      else let
        matchText = elemAt result 0;
        matchLen = stringLength matchText;
      in [matchText (offset + matchLen) (len - matchLen)];

  # }}}
}

# vim: foldmethod=marker:
