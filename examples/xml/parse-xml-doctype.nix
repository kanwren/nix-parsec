{ lib
, nix-parsec
, Name
, Space
, lexeme
}:

with builtins;
with nix-parsec.parsec;

rec {

  QuotedString = (
    alt
    # double quotes
    (
      (skipThen
        (string ''"'') # skip
        (
          thenSkip
          (
            let isChar = c: builtins.match ''[^"]'' c != null; in
            lexeme (takeWhile1 isChar)
          )
          (string ''"'') # skip
        )
      )
    )
    # single quotes
    (
      (skipThen
        (string "'") # skip
        (
          thenSkip
          (
            let isChar = c: builtins.match "[^']" c != null; in
            lexeme (takeWhile1 isChar)
          )
          (string "'") # skip
        )
      )
    )
  );

  DoctypeDeclaration = (
    bind
    (
      skipThen
      (string "<!DOCTYPE") # skip # note: must be uppercase in xml (html is case-insensitive)
      (
        thenSkip
        (
          skipThen
          Space # skip
          (
            thenSkip
            (sequence [
              # values
              Name # name
              (optional (skipThen Space ExternalID)) # externalID
              (optional (skipThen Space IntSubsetExpr)) # intSubset
            ])
            Space # skip
          )
        )
        (string ">") # skip
      )
    )
    (values:
      lib.traceSeq { doctype = values; }
      pure {
        type = "doctype";
        name = elemAt values 0;
        externalID = elemAt values 1;
        intSubset = elemAt values 2;
      }
    )
  );

  ExternalID = (
    alt
    (
      sequence [
        (string "SYSTEM")
        (skipThen Space SystemLiteral)
      ]
    )
    (
      sequence [
        (string "PUBLIC")
        (skipThen Space PubidLiteral)
        (skipThen Space SystemLiteral)
      ]
    )
  );

  SystemLiteral = QuotedString;

  PubidLiteral = QuotedString; # loose
  # strict:
  # PubidLiteral: $ => choice(
  #   seq('"', repeat($._PubidChar), '"'),
  #   seq('\'', repeat($._PubidChar), '\'')
  # ),
  # _PubidChar: $ => /\x20 | \xD | \xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]/,

  IntSubsetExpr = (
    between
    #(string "[")
    #(string "]")
    (thenSkip (string "[") Space)
    (skipThen Space (string "]"))
    (optional IntSubset)
  );

  IntSubset = many1 (
    /*
    alt # TODO sequence?
    MarkupDecl
    DeclSep
    */
    (skipThen Space MarkupDecl)
  );

  #DeclSep = alt PEReference Space; # error: stack overflow (possible infinite recursion)
  DeclSep = alt PEReference Space1;

  PEReference = (
    between
    (string "%")
    (string ";")
    Name
  );

  MarkupDecl = (choice [
    ElementDecl
    /* TODO
    AttlistDecl
    EntityDecl
    NotationDecl
    PI
    Comment
    */
  ]);

  ElementDecl = (
    between
    (string "<!ELEMENT")
    (string ">")
    (
      thenSkip
      (sequence [
        (skipThen Space Name)
        (skipThen Space ContentSpec)
      ])
      Space # skip
    )
  );

  ContentSpec = (choice [
    (string "EMPTY")
    (string "ANY")
    ContentSpecMixed # FIXME error: cannot coerce a function to a string
    ContentSpecChildren
  ]);

  # TODO rename to PCDataNames
  ContentSpecMixed = (choice [
    # PCDATA with no names: ()
    (
      between
      /*
      # wrong: error: cannot coerce a function to a string
      (string (thenSkip "(" Space))
      (string (skipThen Space ")"))
      */
      (thenSkip (string "(") Space)
      (skipThen Space (string ")"))
      (string "#PCDATA")
    )
    # PCDATA with names: ()*
    (
      between
      (thenSkip (string "(") Space)
      (skipThen Space (string ")*"))
      (
        skipThen (string "#PCDATA") (
          (many (
            skipThen Space (
              skipThen (string "|") (
                skipThen Space (
                  Name
                )
              )
            )
          ))
        )
      )
    )
  ]);

  ContentSpecChildren = (
    sequence [
      (alt _choice _seq)
      (optional (
        choice [
          (string "?")
          (string "*")
          (string "+")
        ]
      ))

    ]
  );

  _choice = sequence [
    (string "(")
    Space
    _cp
    (many1 (sequence [
      Space
      (string "|")
      Space
      _cp
    ]))
    Space
    (string ")")
  ];

  _seq = sequence [
    (string "(")
    Space
    _cp
    (many (sequence [
      Space
      (string ",")
      Space
      _cp
    ]))
    Space
    (string ")")
  ];

  _cp = sequence [
    (choice [
      Name
      _choice
      _seq
    ])
    (optional (
      choice [
        (string "?")
        (string "*")
        (string "+")
      ]
    ))
  ];
}
