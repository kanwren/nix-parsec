{
  description = "nix-parsec";

  outputs = { self }:
    {
      lib = {
        lexer = import ./lexer.nix { parsec = self.lib.parsec; };
        parsec = import ./parsec.nix;
      };
    };
}
