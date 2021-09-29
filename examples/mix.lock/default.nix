# Example: parse a yarn.lock file
#
# Load in nix repl and test, e.g.:
#
# nix-repl> parseConfigFile ./yarn.lock
# { type = "success"; value = ...; }

{ pkgs ? import <nixpkgs> { } }:

let
  nix-parsec = import ../../default.nix;
  inherit (nix-parsec) lexer;
  inherit (pkgs) lib;
in

with nix-parsec.parsec;

rec {
  inherit (nix-parsec) parsec;

  charInsideQuotes = c: c != "\"";
  charOutsideQuotes = c: c != "\"" && c != "\n" && c != " " && c != "," && c != ":";
  isIdChar = c: builtins.match "[a-zA-Z0-9_]" c != null;
  unquotedString = takeWhile1 charOutsideQuotes;
  symbol = takeWhile1 isIdChar;
  newLine = string "\n";

  quotedString = between (string "\"") (string "\"") (takeWhile1 charInsideQuotes);

  dependencyName = skipThen (string "  ") (thenSkip quotedString (string ": "));

  
  hex = string ":hex";
  rebar3 = string ":rebar3";
  rebar = string ":rebar";
  mix = string ":mix";
  make = string ":make";
  git = string ":git";
  packageManagersParser = sepBy (alt (alt (alt rebar3 mix) make) rebar) (string ", ") ;
  boolean = alt (string "true") (string "false");

  childDependencies = sepBy (between (string "{") (string "}") childDep ) (string ", ");
  childDep = bind (thenSkip (skipThen (string ":") symbol) (string ", ")) (name:
    bind (thenSkip quotedString (string ", ")) (version:
      bind (between (string "[") (string "]") childDepAttrs) (attrs:
        pure { inherit name version attrs;}
  )));
  childDepAttrs = bind (thenSkip (skipThen (string "hex: :") symbol) (string ", ")) (name:
    bind (thenSkip (skipThen (string "repo: ") quotedString) (string ", ")) (repo:
      bind (skipThen (string "optional: ") boolean) (optional:
        pure { inherit name repo optional;}
  )));

  hexDep = bind (thenSkip hex (string ", :")) (type:
    bind (thenSkip symbol (string ", ")) (name:
      bind (thenSkip quotedString (string ", ")) (version:
        bind (thenSkip quotedString (string ", ")) (firstSha:
          bind (thenSkip (between (string "[") (string "]") packageManagersParser) (string ", ")) (packageManagers:
            bind (thenSkip (between (string "[") (string "]") (optional childDependencies)) (string ", ")) (parsedChildDependencies:
              bind quotedString (registery:
                bind (optional (skipThen (string ", ") quotedString)) (secondSha:
                  pure { inherit type name version firstSha packageManagers parsedChildDependencies registery secondSha; }
                ))))))));

  skipCommaSpace = skipWhile (c: c == "," || c == " ");
  gitDetailsParser  =
    bind (optional (thenSkip (skipThen (string "ref: ") quotedString) skipCommaSpace)) (ref:
      (bind (optional (thenSkip (skipThen (string "branch: ") quotedString) skipCommaSpace)) (branch: 
        (bind (optional (thenSkip (skipThen (string "submodules: ") boolean) skipCommaSpace)) (submodules:
      pure ({} //
            ( if ref == [ ] then {} else { ref = builtins.head ref;}) //
            ( if branch == [ ] then {} else { branch = builtins.head branch;}) //
            ( if submodules == [ ] then {} else { submodules = builtins.head submodules;})
          )
    )))));
    
  gitDep = bind (thenSkip git (string ", ")) (type:
    bind (thenSkip quotedString (string ", ")) (url:
      bind (thenSkip quotedString (string ", ")) (sha:
        bind (between (string "[") (string "]") (optional gitDetailsParser)) (gitDetails:
          pure { inherit type url sha gitDetails;}
   ))));

  dependencyAttrs = between (string "{") (string "},\n") (alt hexDep gitDep);

  dependency = bind dependencyName (name:
    bind dependencyAttrs (attrs: pure (lib.nameValuePair name attrs)));

  dependencyList = many dependency;

  lockFile = between (string "%{\n") (string "}\n") dependencyList;

  parseMixLock = path: runParser lockFile (builtins.readFile path);
}
