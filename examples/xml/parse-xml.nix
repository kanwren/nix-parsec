/*
  xml parser
  based on
  https://github.com/unhammer/tree-sitter-xml
  https://github.com/dorgnarg/tree-sitter-xml
  https://github.com/tree-sitter/tree-sitter-html

  error "expected string x": previous token consumed too many chars, so next token does not match
  -> parsers are overlapping, first parser is too greedy

  TODO why does tree-sitter-html need an external scanner
  to match open-tags with close-tags?
  why does this "just work" here?
  because xml != html? (xml is easier to parse?)

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
  # TODO use callPackage to inherit all values in scope. do we need makeScope?
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
      #attributes = builtins.elemAt values 1;
      attributes = setOfListList (builtins.elemAt values 1);
      children = [];
      name = builtins.elemAt values 0;
      type = "tag";
      nameClose = "";
    }))
  ;

  # TODO bind
  Tag = (bind
    (sequence [
      StartTag
      #(optional CharData)
      (optional Content)
      #(string "a_value") # ok: <a_open>a_value</a_close>
      #Content
      EndTag
    ])
    (values: (pure (let
      startTag = builtins.elemAt values 0;
      name = builtins.elemAt startTag 0;
      attributes = builtins.listToAttrs (builtins.map (keyval: {
        name = builtins.elemAt keyval 0;
        value = builtins.elemAt keyval 1;
      }) (builtins.elemAt startTag 1));
      #children = builtins.elemAt values 1; # FIXME double array
      children = builtins.elemAt (builtins.elemAt values 1) 0; # FIXME double array
      endTag = builtins.elemAt values 2; # FIXME must match startTag
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
    /* debug
    (values: (pure (
      lib.traceSeq { inherit values; v0 = builtins.elemAt values 0; v1 = builtins.elemAt values 1; v2 = builtins.elemAt values 2; }
      values)))
    */
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
        Name # TODO must match StartTag
        # this is nontrivial
        # we must maintain a stack of tags = global parser state
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

  # FIXME expected string '</'
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
  #Content = CharData; # ok: test = parseXml "<k>v</k>";
  #Content = Element; # FIXME

  # FIXME
  /*
  Text = (
    bind
    concatStrings (
      many (
        choice [
          CharData
          Reference
        ]
      )
    )
    (value: pure {
      type = "text";
      inherit value;
    })
  );
  */
  #Text = CharData; # ok
  _Text = (
    concatStrings (
      #many ( # wrong: many = zero or more times -> infinite recursion
      many1 ( # ok: many1 = one or more times
        choice [
          CharData
          Reference
        ]
      )
    )
  );
  # FIXME bind: expected string '/>'
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

        #(string " version=\"1.0\"")
        Attributes

        # wrong: error: attempt to call something which is not a function but a list
        /*
        (
          (optional Space)
          (string "?>")
        )
        */
        # right: sequence [ ... ]

        # wrong: error: list index 2 is out of bounds
        /*
        sequence [
          (optional Space)
          (string "?>")
        ]
        */
        # right: wrap sequence in parens
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
      # wrong: error: value is a function while a list was expected
      /*
      # single quotes
      (
        (string "'")
        VersionNumber
        (string "'")
      )
      */

      # right: sequence [ ... ]
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

  #VersionNumber = matching "1\.[0-9]+";
  VersionNumber = matching "1\\.[0-9]+";
  #VersionNumber = string "1.0";



  Document = bind (sequence [
    Prolog # todo: <?xml ... ?><!DOCTYPE ...>
    # no comment before the main element?
    #Element # optional?
    (optional Element) # optional?
    (many Misc)
    #(optional (many Misc))
    #(optional Space)
  ]) (values: pure {
    type = "root";
    children = builtins.filter (x: x != null) (lib.lists.flatten values);
  });

  #Document = Comment;

in rec {
  # parse node from xml string
  parseXml = runParser (thenSkip Document eof);
  #test = parseXml "<doc attr1='val1' attr2=\"val2&#10;&amp;val2\" />";
  #test = parseXml "<k><k>v</k></k>";
  #test = parseXml "<k>v</k>"; # ok -> CharData "v"
  #test = parseXml "<k><c></c></k>"; # ok
  #test = parseXml "<k a='1'>c1<k a='2'>c2</k>c3</k>"; # FIXME Element <k></k>
  /*
    test = parseXml "<k a='1'>c1<k a='2'>c2</k>c3</k>"; # FIXME Element <k></k>

    value = [
      [ "k" [ [ "a" "1" ] ] ] # <k a='1'>
      [
        # c1<k a='2'>c2</k>c3
        "c1" # c1
        [
          [ "k" [ [ "a" "2" ] ] ] # <k a='2'>
          [ "c2" ] # c2
          "k" # </k>
        ]
        "c3"
      ]
      "k" # </k>
    ];
  */
  #test = parseXml ''<a_open><b_open></b_close></a_close>''; # ok
  #test = parseXml ''<a_open>a_value&amp;a_value<b_open>b_value</b_close></a_close>''; # fixme
  /*
    value =
    {
      attributes = { };
      children =
      [
        [ # TODO remove double array
          "a_value&amp;a_value" # TODO should be set: { type = "text"; value = "..."; }
          {
            attributes = { };
            children = [ [ "b_value" ] ];
            name = "b_open";
            nameClose = "b_close";
            type = "tag";
          }
        ]
      ];
      name = "a_open";
      nameClose = "a_close";
      type = "tag";
    };
  */

  # error: parse error: startTag != endTag: <a_open></a_close>
  #test = printYaml (parseXml ''<a_open>a_value&amp;a_value<b_open>b_value</b_close></a_close>'').value; # fixme

  # error: parse error: startTag != endTag: <b1></b2>
  #test = printYaml (parseXml ''<doc>Doc<a>A</a><b1>B</b2><c1>C</c2><d>D<e>E<f>F<g1/><g2/>F</f>E</e>D</d>Doc</doc>'').value; # error: parse error: startTag != endTag: <b1></b2>

  #test = printYaml (parseXml ''<doc>Doc<a>A</a><b>B</b><c>C</c><d>D<e>E<f>F<g1/><g2/>F</f>E</e>D</d>Doc</doc>'').value; # fixme
  #test = printYaml (parseXml ''<doc>Doc<a>A</a><b>B</b><c>C</c><d>D<e>E<f>F<g1/><g2/>F</f>E</e>D</d>Doc</doc>'').value; # fixme
  #test = printYaml (parseXml ''<doc d1="D1" d2='D2'><!-- comment 1 -->Doc<!-- comment 2 --><a>A</a><b b1="B1" b2='B2'>B</b><c>C</c><d>D<e>E<f>F<g1/><g2/>F</f>E</e>D</d>Doc</doc>'').value; # fixme
  #test =  (parseXml ''<?xml version="1.0"?><doc d1="D1" d2='D2'><!-- comment 1 -->Doc<!-- comment 2 --><a>A</a><b b1="B1" b2='B2'>B</b><c>C</c><d>D<e>E<f>F<g1/><g2/>F</f>E</e>D</d>Doc</doc>'').value; # fixme
  #test =  printYaml (parseXml ''<?xml version="1.0"?><!-- comment 1 --><doc><!-- comment 2 -->text<!-- comment 3 --></doc><!-- comment 4 -->'').value; # fixme
  #test =  printYaml (parseXml ''<node-with-kebab-name/>'').value;
  #test =  printYaml (parseXml ''<node-with-kebab-name a="1" b:b="2">text</node-with-kebab-name>'').value;
  #test =  printYaml (parseXml ''node_with_snake_name/>'').value;
  #test =  printYaml (parseXml ''<node:with:colon:name/>'').value;
  #test =  printYaml (parseXml "<doc>\n</doc>").value;
  #test =  printYaml (parseXml "<doc></doc>\n").value; # fixme
  #test =  printYaml (parseXml (readFile /home/user/src/nixpkgs/pkgs/tools/search/yacy/src/yacy_search_server/ivy.xml)).value; # fixme
  #test =  printYaml (parseXml "<!DOCTYPE xhtml><html></html>").value;
  #test =  printYaml (parseXml ''<!DOCTYPE greeting><doc></doc>'').value;
  #test =  printYaml (parseXml ''<!DOCTYPE greeting []><doc></doc>'').value;
  #test =  printYaml (parseXml ''<!DOCTYPE greeting [ ]><doc></doc>'').value;
  #test =  printYaml (parseXml ''<!DOCTYPE doc1 [ <!ELEMENT elm1 ANY> ]><doc></doc>'').value;
  #test =  printYaml (parseXml ''<!DOCTYPE doc1 [ <!ELEMENT elm1 EMPTY> ]><doc></doc>'').value;
  #test =  printYaml (parseXml ''<!DOCTYPE doc1 [ <!ELEMENT elm1 EMPTY> <!ELEMENT elm2 EMPTY> ]><doc></doc>'').value;
  #test =  printYaml (parseXml ''<!DOCTYPE doc1 [ <!ELEMENT elm1 EMPTY> <!ELEMENT elm2 EMPTY> ]>'').value;
  #test =  printYaml (parseXml ''<!DOCTYPE doc1 [ <!ELEMENT elm1 (#PCDATA)> ]><doc></doc>'').value;
  # fixme: root element is optional, for example <doc>. error: expected string '<'
  #test =  printYaml (parseXml ''<?xml version='1.0' encoding='utf8' standalone="no"?><!DOCTYPE fontconfig SYSTEM "fonts.dtd"><doc></doc>'').value;
  #test =  printYaml (parseXml ''<?xml version='1.0' encoding='utf8' standalone="no"?><!DOCTYPE fontconfig PUBLIC "public id" "fonts.dtd"><doc></doc>'').value;
  test =  printYaml (parseXml "<doc><![CDATAxxxx]]></doc>").value;

  #test =  printYaml (parseXml ''<doc>asdf<!-- asdf --></doc>'').value;
  #test = (parseXml ''<!-- asdf -->'').value;
  #test = printYaml (parseXml ''<!-- asdf -->'').value;
  #test = printNodes (parseXml ''<a_open>a_value&amp;a_value<b_open>b_value</b_close></a_close>'').value; # fixme

  #test = parseXml ''<a_open><b_open>b_value</b_close></a_close>''; # fixme
  /*
    test = parseXml ''<a_open><b_open></b_close></a_close>'';
    value = [
      [ "a_open" [ ] ] # <a_open>
      # children of a_open
      [
        # <b_open></b_close>
        [
          [ "b_open" [ ] ] # <b_open>
          [ ] # children of b_open
          "b_close"
        ]
      ]
      "a_close"
    ];
  */

  # print node to yaml string
  printYaml = import ./print-yaml.nix { inherit lib; };

  # print node to xml string
  printXml = import ./print-xml.nix { inherit lib; };

  #printNodes = nodes: lib.concatStrings (builtins.map printYaml nodes);

  /*
  printNodes = nodes: printNodesInner "" nodes;
  printNodesInner = indent: nodes: lib.concatStrings (
    builtins.map (node: "${node.type},") nodes
  );
  */

}
