# Example: parse a linux kernel config file, like 'surface-4.19.config' in this
# directory
#
# Load in nix repl and test, e.g.:
#
# nix-repl> parseConfigFile ./surface-4.19.config
# { type = "success"; value = ...; }

{ pkgs ? import <nixpkgs> {}
}:

let
  nix-parsec = import ../../default.nix;
  inherit (nix-parsec) lexer;
  inherit (pkgs) lib;
in

with nix-parsec.parsec;

let
  # Consume zero or more spaces, not including newlines
  spaces = skipWhile (c: c == " " || c == "\t");

  # Skip spaces and line comments and newlines
  spaceComments = lexer.space
    (skipWhile1 (c: c == " " || c == "\t" || c == "\n"))
    (lexer.skipLineComment "#")
    fail;

  lexeme = lexer.lexeme spaces;
  symbol = lexer.symbol spaces;

  identifier =
    let isIdChar = c: builtins.match "[a-zA-Z0-9_]" c != null;
    in lexeme (takeWhile1 isIdChar);

  kernelOption = identifier;
  kernelValue = lexeme (choice [
    (fmap (_: lib.kernel.yes) (symbol "y"))
    (fmap (_: lib.kernel.no) (symbol "n"))
    (fmap (_: lib.kernel.module) (symbol "m"))
    (fmap lib.kernel.freeform identifier)
  ]);

  line =
    bind kernelOption (key:
      skipThen
        (symbol "=")
        (thenSkip
          (fmap (lib.nameValuePair key) kernelValue)
          spaceComments));

  configFile =
    fmap lib.listToAttrs
    (skipThen spaceComments (thenSkip (many line) eof));

in {
  parseConfigFile = path: runParser configFile (builtins.readFile path);
}
