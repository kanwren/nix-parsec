# Example: parse a yarn.lock file
#
# Load in nix repl and test, e.g.:
#
# nix-repl> parseYarnLock ./yarn.lock
# { type = "success"; value = ...; }

{ pkgs ? import <nixpkgs> { } }:

let
  nix-parsec = import ../../default.nix;
  inherit (pkgs) lib;
in

with nix-parsec.parsec;

rec {
  inherit (nix-parsec) parsec;

  # Skip spaces and line comments and newlines
  skipEmptyLinesAndComments = 
      skipMany (alt
        (skipWhile1 (c: c == " " || c == "\t" || c == "\n"))
        (skipThen (string "#") (skipWhile (x: x != "\n")))
      );

  charInsideQuotes = c: c != "\"";
  charOutsideQuotes = c: c != "\"" && c != "\n" && c != " " && c != "," && c != ":";
  unquotedString = takeWhile1 charOutsideQuotes;
  newLine = string "\n";

  quotedString = between (string "\"") (string "\"") (takeWhile1 charInsideQuotes);

  version = skipThen (string "  version ") (thenSkip quotedString newLine);
  resolved = skipThen (string "  resolved ") (thenSkip quotedString newLine);
  integrity = skipThen (string "  integrity ") (thenSkip unquotedString newLine);

  dependencyList = thenSkip (sepBy singleDependency newLine) newLine;
  dependencies = skipThen (string "  dependencies:\n") dependencyList;
  optionalDependencies = skipThen (string "  optionalDependencies:\n") dependencyList;

  singleDependency = bind (skipThen (string "    ") (alt quotedString unquotedString)) (parsed_dependency:
    skipThen (string " ") (
      bind quotedString (parsed_version:
        pure { "${parsed_dependency}" = parsed_version; }
      )
    )
  );

  dependencyNames = thenSkip (sepBy (alt quotedString unquotedString) (string ", ")) (string ":\n");

  dependencyAttrs = bind version (parsedVersion:
    bind resolved (parsedResolved:
      bind (optional integrity) (parsedIntegrity:
        bind (optional dependencies) (parsedDependencies:
          bind (optional optionalDependencies) (parsedOptionalDependencies:
            pure (
              { version = parsedVersion; resolved = parsedResolved; }
              //
              (if parsedIntegrity == [ ] then { } else { integrity = builtins.head parsedIntegrity; })
              //
              (if parsedDependencies == [ ] then { } else { dependencies = builtins.head parsedDependencies; })
              //
              (if parsedOptionalDependencies == [ ] then { } else { optionalDependencies = builtins.head parsedOptionalDependencies; })
            )
          )))));
  namesToAttrsList = namesList: dependencyAttrs: map (dependencyName: lib.nameValuePair dependencyName dependencyAttrs) namesList;

  group =
    bind dependencyNames (namesList:
      fmap (parsedAttrs: builtins.listToAttrs (namesToAttrsList namesList parsedAttrs))
        dependencyAttrs);

  configFile =
    (skipThen skipEmptyLinesAndComments
      (thenSkip (sepBy group newLine) eof));

  parseYarnLock = path: runParser configFile (builtins.readFile path);
}
