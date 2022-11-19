# parseXml tests

## parse error: startTag != endTag

```xml
<open>value</close>
```

```
error: parse error: startTag != endTag: <open></close>
```

## tags

```xml
<doc>Doc<a>A</a><b>B</b><c>C</c><d>D<e>E<f>F<g1/><g2/>F</f>E</e>D</d>Doc</doc>
```

```yaml
- root
  children:
    - tag: doc
      children:
        - text: "Doc"
        - tag: a
          children:
            - text: "A"
        - tag: b
          children:
            - text: "B"
        - tag: c
          children:
            - text: "C"
        - tag: d
          children:
            - text: "D"
            - tag: e
              children:
                - text: "E"
                - tag: f
                  children:
                    - text: "F"
                    - tag: g1
                    - tag: g2
                    - text: "F"
                - text: "E"
            - text: "D"
        - text: "Doc"
```

## attributes

```xml
<doc d1="D1" d2='D2'>Doc<a>A</a><b b1="B1" b2='B2'>B</b><c>C</c><d>D<e>E<f>F<g1/><g2/>F</f>E</e>D</d>Doc</doc>
```

```yaml
- root
  children:
    - tag: doc
      attributes:
        d1: "D1"
        d2: "D2"
      children:
        - text: "Doc"
        - tag: a
          children:
            - text: "A"
        - tag: b
          attributes:
            b1: "B1"
            b2: "B2"
          children:
            - text: "B"
        - tag: c
          children:
            - text: "C"
        - tag: d
          children:
            - text: "D"
            - tag: e
              children:
                - text: "E"
                - tag: f
                  children:
                    - text: "F"
                    - tag: g1
                    - tag: g2
                    - text: "F"
                - text: "E"
            - text: "D"
        - text: "Doc"
```

## comments

```xml
<doc><!-- comment 1 -->Doc<!-- comment 2 --><a>A</a><b>B</b><c>C</c><d>D<e>E<f>F<g1/><g2/>F</f>E</e>D</d>Doc</doc>
```

```yaml
- root
  children:
    - tag: doc
      children:
        - comment: " comment 1 "
        - text: "Doc"
        - comment: " comment 2 "
        - tag: a
          children:
            - text: "A"
        - tag: b
          children:
            - text: "B"
        - tag: c
          children:
            - text: "C"
        - tag: d
          children:
            - text: "D"
            - tag: e
              children:
                - text: "E"
                - tag: f
                  children:
                    - text: "F"
                    - tag: g1
                    - tag: g2
                    - text: "F"
                - text: "E"
            - text: "D"
        - text: "Doc"
```

## CDATA

```xml
<doc><![CDATAxxxx]]></doc>
```

```yaml
- root
  children:
    - tag: doc
      children:
        - cdata: "xxxx"
```

## CDATA 2

```xml
<doc><![CDATA
// literally "C data"
#include <stdio.h>
int main(int argc, char **argv) {
  printf("<argc>%i</argc>\n", argc);
  return 0;
}
]]></doc>
```

```yaml
- root
  children:
    - tag: doc
      children:
        - cdata: "\n// literally \"C data\"\n#include <stdio.h>\nint main(int argc, char **argv) {\n  printf(\"<argc>%i</argc>\\n\", argc);\n  return 0;\n}\n"
```

## xml declaration only (loose parser)

```xml
<?xml version="1.0"?>
```

```yaml
- root
  children:
    - decl
      attributes:
        version: "1.0"
```

## xml declaration with document

```xml
<?xml version="1.0"?><doc>text</doc>
```

```yaml
- root
  children:
    - decl
      attributes:
        version: "1.0"
    - tag: doc
      children:
        - text: "text"
```

## node-with-kebab-name

```xml
<node-with-kebab-name/>
```

```yaml
- root
  children:
    - tag: node-with-kebab-name
```

## node_with_snake_name

```xml
<node_with_snake_name/>
```

```yaml
- root
  children:
    - tag: node_with_snake_name
```

## node:with:colon:name

```xml
<node:with:colon:name/>
```

```yaml
- root
  children:
    - tag: node:with:colon:name
```

## attribute-with-kebab-name

```xml
<node attribute-with-kebab-name="value">text</node>
```

```yaml
- root
  children:
    - tag: node
      attributes:
        attribute-with-kebab-name: "value"
      children:
        - text: "text"
```

## attribute_with_snake_name

```xml
<node attribute_with_snake_name="value">text</node>
```

```yaml
- root
  children:
    - tag: node
      attributes:
        attribute_with_snake_name: "value"
      children:
        - text: "text"
```

## attribute:with:colon:name

```xml
<node attribute:with:colon:name="value">text</node>
```

```yaml
- root
  children:
    - tag: node
      attributes:
        attribute:with:colon:name: "value"
      children:
        - text: "text"
```

## newline in doc

```xml
<doc>
</doc>
```

```yaml
- root
  children:
    - tag: doc
      children:
        - text: "\n"
```

## newline after doc

```xml
<doc></doc>

```

```yaml
- root
  children:
    - tag: doc
```

## DOCTYPE

```xml
<!DOCTYPE xhtml><html></html>
```

```yaml
- root
  children:
    - doctype: xhtml
      externalID: []
      intSubset: []
    - tag: html
```

## DOCTYPE ELEMENT ANY

```xml
<!DOCTYPE doc1 [ <!ELEMENT elm1 ANY> ]><doc></doc>
```

```yaml
- root
  children:
    - doctype: doc1
      externalID: []
      intSubset: [[[["elm1","ANY"]]]]
    - tag: doc
```

## DOCTYPE 0 ELEMENT

```xml
<!DOCTYPE doc1 []><doc></doc>
```

```yaml
- root
  children:
    - doctype: doc1
      externalID: []
      intSubset: [[]]
    - tag: doc
```

## DOCTYPE 1 ELEMENT

```xml
<!DOCTYPE doc1 [ <!ELEMENT elm1 EMPTY> ]><doc></doc>
```

```yaml
- root
  children:
    - doctype: doc1
      externalID: []
      intSubset: [[[["elm1","EMPTY"]]]]
    - tag: doc
```

## DOCTYPE 2 ELEMENT

```xml
<!DOCTYPE doc1 [ <!ELEMENT elm1 EMPTY> <!ELEMENT elm2 EMPTY> ]>
```

```yaml
- root
  children:
    - doctype: doc1
      externalID: []
      intSubset: [[[["elm1","EMPTY"],["elm2","EMPTY"]]]]
```

## DOCTYPE ELEMENT PCDATA

```xml
<!DOCTYPE doc1 [ <!ELEMENT elm1 (#PCDATA)> ]>
```

```yaml
- root
  children:
    - doctype: doc1
      externalID: []
      intSubset: [[[["elm1","#PCDATA"]]]]
```

## root element is optional (loose parser)

```xml
<?xml version="1.0" ?>
```

```yaml
- root
  children:
    - decl
      attributes:
        version: "1.0"
```

## DOCTYPE SYSTEM

```xml
<?xml version='1.0' encoding='utf8' standalone="no"?><!DOCTYPE fontconfig SYSTEM "fonts.dtd">
```

```yaml
- root
  children:
    - decl
      attributes:
        encoding: "utf8"
        standalone: "no"
        version: "1.0"
    - doctype: fontconfig
      externalID: [["SYSTEM","fonts.dtd"]]
      intSubset: []
```

## DOCTYPE PUBLIC

```xml
<?xml version='1.0' encoding='utf8' standalone="no"?><!DOCTYPE fontconfig PUBLIC "public id" "fonts.dtd">
```

```yaml
- root
  children:
    - decl
      attributes:
        encoding: "utf8"
        standalone: "no"
        version: "1.0"
    - doctype: fontconfig
      externalID: [["PUBLIC","public id","fonts.dtd"]]
      intSubset: []
```

