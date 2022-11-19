{ lib }:
with builtins;
let
  # print node to xml string
  xmlOfNode = node: (
    if node.type == "tag"
    then (
      "<${node.name}"
      +
      (
        # note: indented string ''...'' strips leading whitespace
        let
          printValue = toJSON; # FIXME encode special chars. example: \n -> &#10;
          printAttribute = name: value: " " + ''${name}=${printValue value}'';
        in
        (lib.concatStrings (lib.mapAttrsToList printAttribute node.attributes))
      )
      +
      (
        if node.nameClose == ""
        then "/>"
        else ">"
      )
      +
      (lib.concatStrings (builtins.map xmlOfNode node.children))
      +
      (
        if node.nameClose == ""
        then ""
        else "</${node.nameClose}>"
      )
    )
    #else node.value # node.type == "text"
    else
    if node.type == "text" then node.value
    else
    if node.type == "comment" then "<!--${node.value}-->"
    else
    if node.type == "root" then (lib.concatStrings (builtins.map xmlOfNode node.children))
    else
    throw "unknown node type ${node.type}"
  );
in
xmlOfNode
