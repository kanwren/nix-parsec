/*

xml comment parser

<document><!-- comment --></document>

xml comments can have arbitrary content, except the string "-->"
so we need a lookahead of 3 chars

*/

let
  nix-parsec = import ../../default.nix;
  inherit (import <nixpkgs> {}) lib;
  inherit (nix-parsec) lexer;
in

with builtins;
with nix-parsec.parsec;

let

  # Consume zero or more characters until the stop string,
  # returning the consumed characters. Cannot fail.
  # based on parsec.takeWhile
  #   :: String -> Parser String
  takeUntil = stop: ps:
    let
      str = elemAt ps 0;
      valueStart = elemAt ps 1;
      len = elemAt ps 2;
      strLen = stringLength str;
      stopLen = stringLength stop;
      seekEnd = position:
        if position >= strLen || (
            let peekStop = substring position stopLen str; in
            #lib.traceSeq { inherit peekStop; }
            (peekStop == stop)
          )
          then position # break
          else seekEnd (position + 1); # continue
      valueEnd = seekEnd valueStart;
      # The number of characters we found
      valueLen = valueEnd - valueStart;
      foundStop = let peekStop = substring valueEnd stopLen str; in
        #lib.traceSeq { inherit peekStop; }
        peekStop == stop;
      value = substring valueStart valueLen str;
      parseEnd = if foundStop then (valueEnd + stopLen) else valueEnd;
      remain = if foundStop then (len - valueLen - stopLen) else (len - valueLen);
    in [value parseEnd remain];

  Comment = (
    bind
    (
      skipThen
      (string "<!--") # skip
      (takeUntil "-->")
    )
    (value: pure { type = "comment"; inherit value; })
  );

in rec {
  parseXmlComment = runParser (thenSkip Comment eof);
  test = (parseXmlComment ''<!-- comment - -- -->'');
  #                            " comment - -- "
}
