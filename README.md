# nix-parsec

Parser combinators in Nix for no-nixpkgs parsing.

### Background

Nix isn't meant to be a general purpose programming language, so using parser
combinators like this should generally be avoided. Here are some reasons to
prefer a pure-Nix parser over other approaches:

- Regular expressions won't work for your use case
  - You _usually_ don't need to parse things in Nix, and when you do, you usually don't need anything more powerful than regex
  - Nix's regex builtins are somewhat inconvenient to use. If you're looking for a more convenient regex interface in Nix, consider [the regex module](https://github.com/chessai/nix-std/blob/master/regex.nix) in [nix-std](https://github.com/chessai/nix-std)
- Parsing performance will not be a bottleneck for the build
  - Nix evaluation can be slow! If your files are large, parsing may take a while
  - If parsing evaluation is a bottleneck, consider implementing your parser in your language of choice and using Nix to invoke it
- It's difficult to pass results of parsing in another language back to Nix
- You need to avoid nixpkgs or other dependencies

### Usage

Include by fetching via usual means (`fetchTarball`, `fetchFromGitHub`, etc.):

```nix
let
  version = "v0.1.0";
  sha256 = "...";

  nix-parsec = import (builtins.fetchTarball {
    url = "https://github.com/nprindle/nix-parsec/archive/${version}.tar.gz";
    inherit sha256;
  });

  inherit (nix-parsec) parsec lexer;
in ...
```

If you are using Nix Flakes, you can add `nix-parsec` as an input:
```nix
{
  inputs = {
    nix-parsec.url = "github:nprindle/nix-parsec";
  };

  outputs = { self, nix-parsec, ... }: {
    # ...
  };
}
```

At the top level, two attribute sets are exported:

- `parsec`: Parser combinators and functions to run parsers
- `lexer`: Combinators for parsing token-related things

The parsing/lexing APIs roughly correspond to those of Haskell's `megaparsec`
library. See `examples/` for some example parsers.

