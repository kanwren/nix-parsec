# A parser is a value with the following type:
#   type Parser a = (String, Int, Int) -> Maybe (a, Int, Int)
#
# - The parameters are the source, the offset, and the length
# - The result is the value produced, the new offset, and the new length
# - If a failure occurs, the result will be 'null'

with builtins;

with rec {
  # Redefine foldr here to avoid depending on lib
  foldr = op: nul: list:
    let
      len = length list;
      fold' = n:
        if n == len
        then nul
        else op (elemAt list n) (fold' (n + 1));
    in fold' 0;
};

rec {
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

  # Query the current offset into the input
  #   :: Parser Int
  offset = ps:
    let
      offset = elemAt res 1;
      len = elemAt res 2;
    in [offset offset len];

  # Query the current length of input remaining
  #   :: Parser Int
  len = ps:
    let
      offset = elemAt res 1;
      len = elemAt res 2;
    in [len offset len];

  # Did a parser fail?
  #   :: Maybe (a, Int, Int) -> Bool
  failed = ps: ps == null;

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
      res1 = parser ps;   # run the first parser
    in if failed res1
      then null
      else let
        val = elemAt res1 0;
        offset = elemAt res1 1;
        len = elemAt res1 2;
      in (f val) [str offset len];

  # Sequence two parsers, ignoring the result of the first one
  #   :: Parser a -> Parser b -> Parser b
  andThen = parser1: parser2: bind parser1 (_: parser2);

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

  # Run a list of parsers, using the first one that succeeds
  #   :: [Parser a] -> Parser a
  choice = foldr alt fail;

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

  # Consume any character
  #   :: Parser Char
  anyChar = satisfy (_: true);

  # Given a string, try to consume it from the input and return it if it suceeds
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

  # Consume 'n' characters, or fail if there's not enough characters left
  #   :: Int -> Parser String
  take = n: ps:
    let
      str = elemAt ps 0;
      offset = elemAt ps 1;
      len = elemAt ps 2;
    in if n <= len
      then [(substring offset n str) (offset + n) (len - n)]
      else null;

  # Consume characters while the predicate holds, returning the consumed
  # characters
  #   :: (Char -> Bool) -> Parser String
  takeWhile = pred: ps:
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
    in [(substring offset numChars str) endIx (len - numChars)];

  # Consume characters while the predicate holds
  #   :: (Char -> Bool) -> Parser null
  skipWhile = pred: ps:
    let
      str = elemAt ps 0;
      offset = elemAt ps 1;
      len = elemAt ps 2;
      # Search for the next offset that violates the predicate
      go = ix:
        if ix >= len || !pred (substring str ix 1)
          then ix
          else go (ix + 1);
      endIx = go offset;
      # The number of characters we found
      numChars = endIx - offset;
    in [null endIx (len - numChars)];

  # Fails if there is still more input remaining, returns null otherwise
  #   :: Parser null
  eof = ps:
    let
      offset = elemAt ps 1;
      len = elemAt ps 2;
    in if len == 0
      then [null offset len]
      else null;

  # Apply a parser zero or more times until it fails, returning a list of the
  # results
  #   :: Parser a -> Parser [a]
  many = parser:
    let go = alt (bind parser (first: fmap (rest: [first] ++ rest) go)) (pure []);
    in go;
  # many = parser: ps:
  #   let
  #     str = elemAt ps 0;
  #     go = ps2:
  #       let res = parser ps2;
  #       in if failed res
  #         then [[] (elemAt ps2 1) (elemAt ps2 2)]
  #         else
  #           let res2 = go [str (elemAt res 1) (elemAt res 2)];
  #           in [([(elemAt res 0)] ++ (elemAt res2 0)) (elemAt res2 1) (elemAt res2 2)];
  #   in go ps;

  # Apply a parser one or more times until it fails, returning a list of the
  # resuls
  #   :: Parser a -> NonEmpty a
  many1 = parser:
    bind parser (first: fmap (rest: [first] ++ rest) (many parser));

  # Repeat a parser 'n' times, returning the results from each parse
  #   :: Int -> Parser a -> Parser [a]
  replicate = n: parser:
    let go = m: if m == 0
      then pure []
      else bind parser (first: fmap (rest: [first] ++ rest) (go (m - 1)));
    in go n;

  # 'notFollowedBy p' only succeeds when 'p' fails, and never consumes any input
  #   :: Parser a -> Parser ()
  notFollowedBy = parser: ps:
    let
      offset = elemAt ps 1;
      len = elemAt ps 2;
    in if failed (parser ps)
      then [null offset len]
      else null;

  # Examine the next character without consuming it. Fails if there's no input
  # left.
  #   :: Int -> Parser String
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
}
