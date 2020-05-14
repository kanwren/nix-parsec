let
  parsec = import ./parsec.nix;
  lexer = import ./lexer.nix { inherit parsec; };
  regex = import ./regex.nix;
in {
  inherit parsec lexer regex;
}
