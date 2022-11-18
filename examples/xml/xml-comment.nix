/*

xml comment parser
a short demo for parsec.peek

<doc><!-- asdf --></doc>

xml comments can have arbitrary content, except the string "-->"
so we need a lookahead of 3 chars = (peekN 3)

*/

let
  nix-parsec = import ../../default.nix;
  inherit (import <nixpkgs> {}) lib;
  inherit (nix-parsec) lexer;
in

with nix-parsec.parsec;

let
  Spaces = skipWhile (c: c == " ");
  lexeme = lexer.lexeme Spaces;

  # Examine the next N characters without consuming them.
  # Fails if there is not enough input left (TODO verify).
  #   :: Parser String
  peekN = n: ps:
    let
      str = builtins.elemAt ps 0;
      offset = builtins.elemAt ps 1;
      len = builtins.elemAt ps 2;
    in if len >= n
      then [(builtins.substring offset n str) offset len]
      else {
        context = "parsec.peek";
        msg = "expected ${n} characters";
      };

  concatStrings = parser: bind parser (values: pure (lib.concatStrings values));

  # init = all except last. fix: cannot coerce null to a string
  concatInitStrings = parser: bind parser (values: pure (lib.concatStrings (lib.lists.init values)));

  Comment = (
    skipThen
    (string "<!--") # skip
    (
      concatInitStrings (
        many (
          alt
          (
            let
              isChar = c:
                lib.traceSeq ("c='${c}' -> ${if c != "-" then "take" else "skip"}")
                (c != "-")
              ;
            in
            lexeme (takeWhile1 isChar)
          )
          (
            bind (peekN 3) (value:
              if value == "-->"
              then lib.traceSeq "peek: value='${value}' -> skip 3" skip 3 # skip returns null
              else lib.traceSeq "peek: value='${value}' -> take 1" take 1
            )
          )
        )
      )
    )
  );

in {
  parseXmlComment = runParser (thenSkip Comment eof);
}
