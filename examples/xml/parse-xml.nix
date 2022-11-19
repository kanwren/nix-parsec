/*
  xml parser
  based on
  https://github.com/unhammer/tree-sitter-xml
  https://github.com/dorgnarg/tree-sitter-xml
  https://github.com/tree-sitter/tree-sitter-html

  TODO decode entities like &#10; -> \n
  TODO encode entities like \n -> &#10;
  TODO xpath selector parser + compiler
  TODO css selector parser + compiler
*/

let
  nix-parsec = import ../../default.nix;
  inherit (import <nixpkgs> {}) lib;
  inherit ((import <nixpkgs> {}).pkgs) callPackage;
  inherit (nix-parsec) lexer;
in

with builtins;
with nix-parsec.parsec;

let
  # TODO bind all values from doctype
  # TODO use lib.makeScope + pkgs.callPackage to inherit all values in scope
  inherit (import ./parse-xml-doctype.nix { inherit lib nix-parsec Name Space lexeme; }) DoctypeDeclaration;
  #inherit (callPackage ./parse-xml-doctype.nix {}) DoctypeDeclaration;

  isSpace = c: c == " " || c == "\n" || c == "\t";
  Space = skipWhile isSpace; # skipWhile: zero or more characters
  Space1 = skipWhile1 isSpace; # skipWhile1: one or more characters
  lexeme = lexer.lexeme Space;
  symbol = lexer.symbol Space;

  concatStrings = parser: bind parser (values: pure (lib.concatStrings values));

  # init = all except last
  concatInitStrings = parser: bind parser (values: pure (lib.concatStrings (lib.lists.init values)));

  Element =
    alt
    EmptyTag
    Tag
  ;

  Name =
    let isChar = c: builtins.match "[a-zA-Z0-9_:-]" c != null; in
    lexeme (takeWhile1 isChar)
  ;

  Hex =
    let isChar = c: builtins.match "[0-9a-fA-F]" c != null; in
    lexeme (takeWhile1 isChar)
  ;

  #Digits = lexer.decimal;
  Digits =
    let isChar = c: builtins.match "[0-9]" c != null; in
    lexeme (takeWhile1 isChar)
  ;

  # FIXME allow empty value?
  CharData =
    let isChar = c: builtins.match "[^<&]" c != null; in
    lexeme (takeWhile1 isChar)
  ;

  # this can be anything
  Chars =
    let isChar = c: builtins.match "." c != null; in
    lexeme (takeWhile1 isChar)
  ;

  # TODO rename to CharDataInDoubleQuotes
  # FIXME allow empty value?
  ValueChunkInDoubleQuotes =
    let isChar = c: builtins.match "[^<&\"]" c != null; in
    lexeme (takeWhile1 isChar)
  ;

  # TODO rename to CharDataInSingleQuotes
  # FIXME allow empty value?
  ValueChunkInSingleQuotes =
    let isChar = c: builtins.match "[^<&']" c != null; in
    lexeme (takeWhile1 isChar)
  ;

  EmptyTag =
    bind
    (
      skipThen
      (symbol "<") # skip
      (
        thenSkip
        (
          # TODO refactor: StartTag + EmptyTag
          sequence [
            Name
            Attributes
          ]
        )
        (sequence [ (optional Space) (string "/>") ]) # skip
      )
    )
    # TODO refactor: StartTag + EmptyTag
    (values: (pure {
      # sorted by alphabet
      attributes = setOfListList (builtins.elemAt values 1);
      children = [];
      name = builtins.elemAt values 0;
      type = "tag";
      nameClose = "";
    }))
  ;

  Tag = (bind
    (sequence [
      StartTag
      #(optional CharData)
      (optional Content)
      EndTag
    ])
    (values: (pure (let
      startTag = builtins.elemAt values 0;
      name = builtins.elemAt startTag 0;
      attributes = builtins.listToAttrs (builtins.map (keyval: {
        name = builtins.elemAt keyval 0;
        value = builtins.elemAt keyval 1;
      }) (builtins.elemAt startTag 1));
      children = builtins.elemAt (builtins.elemAt values 1) 0;
      endTag = builtins.elemAt values 2;
    in (
      #assert (name == endTag);
      if (name != endTag)
      # TODO print source location
      then throw "parse error: startTag != endTag: <${name}></${endTag}>"
      else
      {
        inherit attributes children name;
        nameClose = endTag; # debug
        type = "tag";
      }
    ))))
  );

  StartTag =
    skipThen
    (symbol "<") # skip
    (
      thenSkip
      (
        # TODO refactor: StartTag + EmptyTag
        sequence [
          Name
          Attributes
        ]
      )
      (sequence [ (optional Space) (string ">") ]) # skip
    )
  ;

  EndTag =
    skipThen
    (symbol "</") # skip
    (
      thenSkip
      (
        Name
      )
      (sequence [ (optional Space) (string ">") ]) # skip
    )
  ;

  Attributes = (
    many (
      skipThen
      Space # skip
      Attribute
    )
  );

  Attribute = sequence [
    Name # key
    (
      skipThen
      (symbol "=") # skip
      AttributeValue
    )
  ];

  AttributeValue = (alt
    # double quotes
    (
      skipThen
      (symbol ''"'') # skip
      (
        thenSkip
        (
          concatStrings (many (
            alt
            ValueChunkInDoubleQuotes # /[^<&"]/
            Reference
          ))
        )
        (symbol ''"'') # skip
      )
    )
    # single quotes
    (
      skipThen
      (symbol "'") # skip
      (
        thenSkip
        (
          concatStrings (many (
            alt
            ValueChunkInSingleQuotes # /[^<&']/
            Reference
          ))
        )
        (symbol "'") # skip
      )
    )
  );

  Reference = (alt
    EntityReference
    CharacterReference
  );

  EntityReference = concatStrings (sequence [
    (symbol "&") # TODO symbol or string
    Name
    (symbol ";")
  ]);

  CharacterReference = (concatStrings (alt
    (sequence [
      (symbol "&#") # TODO symbol or string
      Digits # /[0-9]+/
      (symbol ";")
    ])
    (sequence [
      (symbol "&#x") # TODO symbol or string
      Hex # /[0-9a-fA-F]+/
      (symbol ";")
    ])
  ));

  Content = many (
    choice [
      Element
      Text
      Comment
      CdataSection
      /* TODO
      ProcessingInstructions
      */
    ]
  );

  Text = (
    bind
    (
      concatStrings (
        many1 (
          choice [
            CharData
            Reference
          ]
        )
      )
    )
    (value: pure {
      type = "text";
      inherit value;
      #children = [];
    })
  );

  /* not used
  # Examine the next N characters without consuming them.
  # Fails if there is not enough input left.
  #   :: Parser String
  peekN = n: ps:
    let
      str = builtins.elemAt ps 0;
      offset = builtins.elemAt ps 1;
      len = builtins.elemAt ps 2;
    in if len >= n
      then [(builtins.substring offset n str) offset len]
      else {
        context = "parsec.peekN";
        msg = "expected ${n} characters";
      };
  */

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
      # Search for the next valueStart that violates the predicate
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

  CdataSection = (
    bind
    (
      skipThen
      (string "<![CDATA") # skip
      (takeUntil "]]>")
    )
    (value: pure { type = "cdata"; inherit value; })
  );

  Misc = choice [
    Comment
    #ProcessingInstructions # todo: <? ... ?>
    #Space # FIXME error: stack overflow (possible infinite recursion)
    Space1 # returns null
  ];

  Prolog = sequence [
    (optional XMLDeclaration)
    (optional Misc)
    (optional (sequence [
      DoctypeDeclaration
      (many Misc)
    ]))
  ];

  /*
  # strict: allow only some attributes
  XMLDeclaration = sequence [
    #(string ''<?xml version="1.0"?>'') # ok
    # fixme
    (string "<?xml")
    #(string (" " + ''version="1.0"'')) # ok
    VersionInfo # FIXME error: value is a function while a list was expected
    #(optional EncodingDeclaration) # TODO
    #(optional SDDeclaration) # TODO
    (optional Space)
    (string "?>")
  ];
  */

  # loose: allow all attributes
  XMLDeclaration = (
    bind
    (
      skipThen
      (string "<?xml")
      (
        thenSkip
        Attributes
        (sequence [
          (optional Space)
          (string "?>")
        ])
      )
    )
    (
      values:
      #lib.traceSeq { inherit values; }
      pure {
        type = "decl";
        attributes = setOfListList values;
      }
    )
  );

  # setOfListList [ ["a" 1] ["b" 2] ] == { a=1; b=2; }
  # TODO shorter? go directly from list to set?
  # aka: listListToAttrs
  setOfListList = list: builtins.listToAttrs (builtins.map (keyval: {
    name = builtins.elemAt keyval 0;
    value = builtins.elemAt keyval 1;
  }) list);

  VersionInfo = sequence [
    Space
    (string "version=")
    (
      alt
      # single quotes
      (sequence [
        (string "'")
        VersionNumber
        (string "'")
      ])
      # double quotes
      (sequence [
        (string ''"'')
        VersionNumber
        (string ''"'')
      ])
    )
  ];

  VersionNumber = matching "1\\.[0-9]+";

  Document = bind (sequence [
    Prolog # todo: <?xml ... ?><!DOCTYPE ...>
    # no comment before the main element?
    #Element # optional?
    (optional Element) # optional?
    (many Misc)
  ]) (values: pure {
    type = "root";
    children = builtins.filter (x: x != null) (lib.lists.flatten values);
  });

in rec {
  # parse node from xml string
  parseXml = runParser (thenSkip Document eof);

  # print node to yaml string
  printYaml = import ./print-yaml.nix { inherit lib; };

  # print node to xml string
  printXml = import ./print-xml.nix { inherit lib; };
}
