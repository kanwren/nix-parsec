let
  parsec = import ./parsec.nix;
  lexer = import ./lexer.nix { inherit parsec; };
in {
  inherit parsec lexer;
}
