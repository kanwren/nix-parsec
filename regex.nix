with rec {
  concatStrings = builtins.concatStringsSep "";

  # Replace the substitutions in a replacement string with the appropriate
  # values from the captures.
  replaceCaptures = rep: captures:
    let
      subs = builtins.split ''\\([[:digit:]]+)'' rep;
      replaceGroup = group:
        if builtins.isList group
          then builtins.elemAt captures (builtins.fromJSON (builtins.head group))
          else group;
    in concatStrings (builtins.map replaceGroup subs);
};

rec {
  # Perform regex substitution on a string. For each match, run a function on
  # the resulting capture groups to determine the substitution text.
  #
  # Example: replace vowel sequences with their lengths
  #
  # countVowels = str:
  #   let
  #     toCount = groups:
  #       toString (builtins.stringLength (builtins.head groups));
  #   in replaceFunc "([aeiou]+)" toCount str;
  replaceFunc = regex: f: str:
    let
      groups = builtins.split regex str;
      replaceGroup = group:
        if builtins.isList group
          then f group
          else group;
    in concatStrings (builtins.map replaceGroup groups);

  # Perform regex substitution on a string. If the regex contains capture
  # groups, the replacing string can refer to them with \0, \1, etc.
  #
  # Example: duplicate all vowels in a string
  #
  # dupVowels = replace ''([aeiou])'' ''\0\0'';
  replace = regex: rep:
    let substituteCaptures = replaceCaptures rep;
    in replaceFunc regex substituteCaptures;
}
