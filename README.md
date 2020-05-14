# nix-parsec

Parser combinators in Nix. Nix isn't meant to be a general purpose programming
language, so using parser combinators like this should really be avoided,
unless:

- Nix's built-in regex tools won't work for your use case
  - You _usually_ don't need to parse things in Nix, and when you do, you
    usually don't need anything more powerful than regex
- Parsing performance will not be a bottleneck for the build
  - If it is, consider implementing your parser in your language of choice and
    using nix to invoke it
- It's difficult to pass results of parsing in another language back to Nix

Don't ask what I actually needed this for.

### Usage

Include by fetching via normal means. At the top level, three attribute sets are
exported:

- `parsec`: Parser combinators and functions to run parsers
- `lexer`: Combinators for parsing token-related things
- `regex`: Helper functions for working with regular expressions

The parsing/lexing APIs roughly corresponds to those of Haskell's `megaparsec`
library.

