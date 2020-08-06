# A parser is a value with the following type:
#   type Parser a = (String, Int, Int) -> Either e (a, Int, Int)
#
# - The parameters are the source, the offset, and the length
# - The result is the value produced, the new offset, and the new length
# - If a failure occurs, the result will be an attribute set containing
#   information about the error
#
# Note that in the types, 'Maybe a' denotes a value that is either null or a
# singleton list containing a value of type 'a'. 'NonEmpty a' denotes a list
# containing one or more values of type 'a'. 'null' denotes the singleton type
# containing only the value 'null'.

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

  # Run a parser, returning the result in an attrset either containing a "type"
  # key and a "value" key. If "type" is "error", "value" will contain the
  # reported error. If "type" is "success", "value" will contain the parsed
  # value.
  #
  # If the parser did not consume all of its input, this will still succeed. If
  # you want to make sure all input has been consume, use 'eof'.
  #
  #   :: Parser a -> String -> Either e a
  runParser = parser: str:
    let res = parser [str 0 (stringLength str)];
    in if failed res
      then { type = "error"; value = res; }
      else { type = "success"; value = elemAt res 0; };

  # }}}

  # queries {{{

  # Did the raw result of a parser fail?
  #
  #   :: Either e (a, Int, Int) -> Bool
  failed = ps: !builtins.isList ps;

  # Query the current state of the parser
  #   :: Parser (String, Int, Int)
  state = ps:
    let
      offset = elemAt ps 1;
      len = elemAt ps 2;
    in [ps offset len];

  # Augment a parser to also return the number of characters it consumed
  #   :: Parser a -> Parser (Int, a)
  measure = parser: ps:
    let
      initialOffset = elemAt ps 1;
      res = parser ps;
    in if failed res
      then res
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
      then res
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
      then res
      else [(f val) offset len];

  # Lift a value into a parser
  #   :: a -> Parser a
  pure = x: ps: [x (elemAt ps 1) (elemAt ps 2)];

  # Applicative functor application
  #   :: Parser (a -> b) -> Parser a -> Parser b
  ap = p1: p2: bind p1 (f: fmap f p2);

  # Lift a two-argument function over two parsers in sequence.
  #   :: Parser (a -> b) -> Parser a -> Parser b
  lift2 = f: p1: p2: ap (fmap f p1) p2;

  # Monadic bind; sequence two parsers together
  #   :: Parser a -> (a -> Parser b) -> Parser b
  bind = parser: f: ps:
    let
      str = elemAt ps 0;
      res1 = parser ps;
    in if failed res1
      then res1
      else let
        val = elemAt res1 0;
        offset = elemAt res1 1;
        len = elemAt res1 2;
      in (f val) [str offset len];

  # Sequence two parsers, ignoring the result of the first one, like '*>' in
  # Haskell
  #   :: Parser a -> Parser b -> Parser b
  skipThen = parser1: parser2: bind parser1 (_: parser2);

  # Sequence two parsers, ignoring the result of the second one, like '<*' in
  # Haskell
  #   :: Parser a -> Parser b -> Parser a
  thenSkip = parser1: parser2: bind parser1 (x: fmap (_: x) parser2);

  # Run a list of parsers in sequence, collecting their results
  #   :: [Parser a] -> Parser [a]
  sequence = xs:
    let
      len = length xs;
      go = n:
        if n >= len
          then pure []
          else bind (elemAt xs n) (first: fmap (rest: [first] ++ rest) (go (n + 1)));
    in go 0;

  # Ignore the results of a parser
  #   :: Parser a -> Parser null
  void = fmap (_: null);

  # Like 'sequence', but ignore the outputs of the parsers
  #   :: [Parser a] -> Parser null
  sequence_ = xs:
    let
      len = length xs;
      go = n:
        if n >= len
          then pure null
          else skipThen (elemAt xs n) (go (n + 1));
    in go 0;

  # }}}

  # options and failure {{{

  # Parser that always fails (the identity under 'alt')
  fail = failWith { source = "parsec.fail"; };

  # Parser that always fails with the given error
  #   :: e -> Parser a
  failWith = e: _: e;

  # Apply a function to modify an error message for a parser
  annotate = f: parser: ps:
    let res = parser ps;
    in if failed res
      then f res
      else res;

  # Modify a parser error message by adding the given attributes
  #
  # NOTE: this overrides any of the old attributes, so make sure that any
  # possible error information is irrelevant first
  annotateWith = e: annotate (x: x // e);

  # Add a new source annotation to an error, keeping the old error entirely
  annotateSource = s: annotate (e: { source = s; error = e; });

  # Add information about the current offset to a parser
  #
  # NOTE: this overrides any old offset info, which could make it confusing
  # where the error actually happened.
  withOffsetInfo = parser:
    bind state (info: annotateWith { str = elemAt info 0; offset = elemAt info 1; } parser);

  # Override an error message for a parser
  label = e: annotate (_: e);

  # Run two parsers; if the first one fails, run the second one
  #   :: Parser a -> Parser a -> Parser a
  alt = parser1: parser2: withOffsetInfo (ps:
    let
      str = elemAt ps 0;
      res1 = parser1 ps;
      res2 = parser2 ps;
    in if failed res1
      then if failed res2
        then { source = "parsec.alt"; error = [res1 res2]; }
        else res2
      else res1);

  # Try to apply a parser, or return a default value if it fails without
  # consuming input. Cannot fail.
  #   :: a -> Parser a -> Parser a
  option = def: parser: alt parser (pure def);

  # Try to apply a parser. If it succeeds, return its result in a singleton
  # list, and if it fails without consuming input, return an empty list. Cannot
  # fail.
  #   :: Parser a -> Parser [a]
  optional = parser: alt (fmap (x: [x]) parser) (pure []);

  # Run a list of parsers, using the first one that succeeds
  #   :: [Parser a] -> Parser a
  choice = parsers: withOffsetInfo (ps:
    let
      results = map (p: p ps) parsers;
      firstSuccess = foldr (x: rest: if failed x then rest else x) null results;
    in if firstSuccess == null
      then {
        source = "parsec.choice";
        msg = "expected one of these to be satisfied";
        error = results;
      }
      else firstSuccess);

  # }}}

  # consumption primitives {{{

  # Consumes a character if it satisfies a predicate
  #   :: (Char -> Bool) -> Parser Char
  satisfy = pred: withOffsetInfo (ps:
    let
      str = elemAt ps 0;
      offset = elemAt ps 1;
      len = elemAt ps 2;
      c = substring offset 1 str; # the next character
    in if len > 0 && pred c
      then [c (offset + 1) (len - 1)]
      else { source = "parsec.satisfy"; });

  # Consumes a character if it satisfies a predicate, applying a function to the
  # result.
  #   :: (Char -> a) -> (Char -> Bool) -> Parser a
  satisfyWith = f: pred: withOffsetInfo (ps:
    let
      str = elemAt ps 0;
      offset = elemAt ps 1;
      len = elemAt ps 2;
      c = substring offset 1 str; # the next character
    in if len > 0 && pred c
      then [(f c) (offset + 1) (len - 1)]
      else { source = "parsec.satisfyWith"; });

  # Consume any character
  #   :: Parser Char
  anyChar = withOffsetInfo (annotateWith {
    source = "parsec.anyChar";
  } (satisfy (_: true)));

  # Consume any character except a given character
  #   :: Char -> Parser Char
  anyCharBut = c: withOffsetInfo (annotateWith {
    source = "parsec.anyCharBut";
    error = "expected any char except ${c}";
  } (satisfy (x: x != c)));

  # Given a string, try to consume it from the input and return it if it
  # succeeds. If it fails, DON'T consume any input.
  #   :: String -> Parser String
  string = pr: withOffsetInfo (ps:
    let
      prefixLen = stringLength pr;
      str = elemAt ps 0;
      offset = elemAt ps 1;
      len = elemAt ps 2;
    in if len >= prefixLen && substring offset prefixLen str == pr
      then [pr (offset + prefixLen) (len - prefixLen)]
      else {
        source = "parsec.string";
        msg = "expected string '${pr}'";
      });

  # 'notFollowedBy p' only succeeds when 'p' fails, and never consumes any input
  #   :: Parser a -> Parser null
  notFollowedBy = parser: withOffsetInfo (ps:
    let
      offset = elemAt ps 1;
      len = elemAt ps 2;
    in if failed (parser ps)
      then [null offset len]
      else { source = "parsec.notFollowedBy"; });

  # Fails if there is still more input remaining, returns null otherwise
  #   :: Parser null
  eof = withOffsetInfo (ps:
    let
      offset = elemAt ps 1;
      len = elemAt ps 2;
    in if len == 0
      then [null offset len]
      else {
        source = "parsec.eof";
        msg = "expected end of input";
      });

  # Return whether or not we're at the end of the input. Cannot fail.
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
    let p' =
      let go = m: if m == 0
        then pure []
        else bind parser (first: fmap (rest: [first] ++ rest) (go (m - 1)));
      in go n;
    in annotate (e: {
      source = "parsec.count";
      msg = "expected ${toString n} occurrances";
      error = e;
    }) p';

  # Consume 'n' characters, or fail if there's not enough characters left.
  # Return the characters consumed.
  #   :: Int -> Parser String
  take = n: assert n >= 0; withOffsetInfo (ps:
    let
      str = elemAt ps 0;
      offset = elemAt ps 1;
      len = elemAt ps 2;
    in if n <= len
      then [(substring offset n str) (offset + n) (len - n)]
      else {
        source = "parsec.take";
        error = "expected ${toString n} characters, but only got ${toString len}";
      });

  # Consume zero or more characters while the predicate holds, returning the
  # consumed characters. Cannot fail.
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
    let p' = bind (satisfy pred) (first: fmap (rest: first + rest) (takeWhile pred));
    in annotateWith {
      source = "parsec.takeWhile1";
      msg = "expected at least one character matching the predicate";
    } p';

  # Apply a parser zero or more times until it fails, returning a list of the
  # results. Cannot fail.
  #   :: Parser a -> Parser [a]
  many = parser:
    let go = alt (bind parser (first: fmap (rest: [first] ++ rest) go)) (pure []);
    in go;

  # Apply a parser one or more times until it fails, returning a list of the
  # results
  #   :: Parser a -> Parser (NonEmpty a)
  many1 = parser:
    let p' = bind parser (first: fmap (rest: [first] ++ rest) (many parser));
    in annotate (e: {
      source = "parsec.many1";
      msg = "expected one or more occurrences";
      error = e;
    }) p';

  # Repeat a parser zero or more times until the end parser succeeds. Returns a
  # list of the results of the first parser.
  #   :: Parser a -> Parser b -> Parser [a]
  manyTill = parser: end:
    let p' =
      let go = alt (fmap (_: []) end) (bind parser (first: fmap (rest: [first] ++ rest) go));
      in go;
    in annotateSource "parsec.manyTill" p';

  # Repeat a parser one or more times until the end parser succeeds. Returns a
  # list of the results of the first parser.
  #   :: Parser a -> Parser b -> Parser (NonEmpty a)
  manyTill1 = parser: end:
    let p' = bind parser (first: fmap (rest: [first] ++ rest) (manyTill parser end));
    in annotateSource "parsec.manyTill1" p';

  # }}}

  # separators {{{

  # Sequence three parsers, 'before', 'after', and 'middle', running them in the
  # obvious order and keeping the middle result.
  # Example: parens = between (string "(") (string ")")
  #
  #   :: Parser a -> Parser b -> Parser c -> Parser c
  between = before: after: middle: skipThen before (thenSkip middle after);

  # Parse zero or more occurrences of the first parser, separated by the second
  # parser. Returns a list of results of the first parser. Cannot fail.
  #   :: Parser a -> Parser b -> Parser [a]
  sepBy = parser: end:
    alt (sepBy1 parser end) (pure []);

  # Parse one or more occurrences of the first parser, separated by the second
  # parser. Returns a list of results of the first parser.
  #   :: Parser a -> Parser b -> Parser (NonEmpty a)
  sepBy1 = parser: end:
    let p' = bind parser (first: fmap (rest: [first] ++ rest) (many (skipThen end parser)));
    in annotateSource "parsec.sepBy1" p';

  # Parse zero or more occurrences of the first parser, separated and ended by
  # the second parser. Returns a list of results of the first parser. Cannot
  # fail.
  #   :: Parser a -> Parser b -> Parser [a]
  endBy = parser: end:
    many (thenSkip parser end);

  # Parse one or more occurrences of the first parser, separated and ended by
  # the second parser. Returns a list of results of the first parser.
  #   :: Parser a -> Parser b -> Parser (NonEmpty a)
  endBy1 = parser: end:
    let p' = many1 (thenSkip parser end);
    in annotateSource "parsec.endBy1" p';

  # Parse zero or more occurrences of the first parser, separated and optionally
  # ended by the second parser. Returns a list of result of the first parser.
  # Cannot fail.
  #   :: Parser a -> Parser b -> Parser [a]
  sepEndBy = parser: end:
    alt (sepEndBy1 parser end) (pure []);

  # Parse one or more occurrences of the first parser, separated and optionally
  # ended by the second parser. Returns a list of result of the first parser.
  #   :: Parser a -> Parser b -> Parser (NonEmpty a)
  sepEndBy1 = parser: end:
    let p' =
      let
        go = alt
          (skipThen end (alt
            (bind parser (first: fmap (rest: [first] ++ rest) go))
            (pure [])))
          (pure []);
      in bind parser (first: fmap (rest: [first] ++ rest) go);
    in annotateSource "parsec.sepEndBy1" p';

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
      else {
        source = "parsec.skip";
        error = "expected ${toString n} characters, but only got ${toString len}";
      };

  # Consume zero or more characters while the predicate holds. Cannot fail.
  #   :: (Char -> Bool) -> Parser null
  skipWhile = pred: ps:
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
    in [null endIx (len - numChars)];

  # Consume one or more characters while the predicate holds.
  #   :: (Char -> Bool) -> Parser null
  skipWhile1 = pred:
    let p' = skipThen (satisfy pred) (skipWhile pred);
    in annotateSource "parsec.skipWhile1" p';

  # Run a parser zero or more times until it fails, discarding all the input
  # that it accepts. Cannot fail.
  #   :: Parser a -> Parser null
  skipMany = parser:
    let go = alt (skipThen parser go) (pure null);
    in go;

  # Run a parser one or more times until it fails, discarding all the input that
  # it accepts.
  #   :: Parser a -> Parser null
  skipMany1 = parser:
    let p' = skipThen parser (skipMany parser);
    in annotateSource "parsec.skipMany1" p';

  # Repeat a parser zero or more times until the end parser succeeds. Discards
  # consumed input.
  #   :: Parser a -> Parser b -> Parser null
  skipTill = parser: end:
    let p' =
      let go = alt end (skipThen parser go);
      in void go;
    in annotateSource "parsec.skipTill" p';

  # Repeat a parser one or more times until the end parser succeeds. Discards
  # consumed input.
  #   :: Parser a -> Parser b -> Parser null
  skipTill1 = parser: end:
    let p' = skipThen parser (skipTill parser end);
    in annotateSource "parsec.skipTill1" p';

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
      else {
        source = "parsec.peek";
        msg = "expected a character";
      };

  # Examine the rest of the input without consuming it. Cannot fail.
  #
  # NOTE: this has to copy the rest of the input into a substring, so use with
  # caution.
  #
  #   :: Parser String
  peekRest = ps:
    let
      str = elemAt ps 0;
      offset = elemAt ps 1;
      len = elemAt ps 2;
    in [(substring offset len str) offset len];

  # Consume and return the rest of the input. Cannot fail.
  #
  # NOTE: this has to copy the rest of the input into a substring, so use with
  # caution.
  #
  #   :: Parser String
  consumeRest = ps:
    let
      str = elemAt ps 0;
      offset = elemAt ps 1;
      len = elemAt ps 2;
    in [(substring offset len str) (offset + len) 0];

  # Consume and ignore the rest of the input. Cannot fail.
  #   :: Parser null
  dropRest = ps:
    let
      offset = elemAt ps 1;
      len = elemAt ps 2;
    in [null (offset + len) 0];

  # }}}

  # regex {{{

  # Given a regex that matches a string, consume characters matching that regex,
  # or fail if the next characters in the input do not match. Return the matched
  # text, followed by any capture groups from the match.
  #
  # NOTE: This has to copy the rest of the string, so if you know the maximum
  # number of characters you may need, use "matchingN".
  #
  #   :: String -> Parser (NonEmpty String)
  matching = regex: annotateSource "parsec.matching" (ps:
    let len = elemAt ps 2;
    in matchingN len regex ps);

  # Given a regex that matches a string, consume at most 'n' characters from the
  # input matching the regular expression. Return the matched text, followed by
  # any capture groups from the match.
  #   :: Int -> String -> Parser (NonEmpty String)
  matchingN = n: assert n >= 0; regex: withOffsetInfo (ps:
    let
      str = elemAt ps 0;
      offset = elemAt ps 1;
      len = elemAt ps 2;
      result = match ("(" + regex + ").*") (substring offset n str);
    in if result == null
      then {
        source = "parsec.matchingN";
        error = "expected text matching '${regex}'";
      }
      else let
        matchText = elemAt result 0;
        matchLen = stringLength matchText;
      in [result (offset + matchLen) (len - matchLen)]);

  # }}}
}

# vim: foldmethod=marker:
