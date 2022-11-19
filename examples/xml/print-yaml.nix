{ lib }:
with builtins;
let
  # print node to yaml string
  yamlOfNode = let
    yamlOfNodeInner = indent: node: (
      if (node ? type) then
      (
        if (node.type == "tag" || node.type == "decl" || node.type == "doctype" || node.type == "root")
        then (
          (
            #if node.nameClose == ""
            #then "${indent}- tag: <${node.name}/>\n"
            #else "${indent}- tag: <${node.name}></${node.nameClose}>\n"
            (
              if (node.type == "tag" || node.type == "doctype")
              then "${indent}- ${node.type}: ${node.name}\n"
              else "${indent}- ${node.type}\n"
            )
          )
          +
          (
            if (node.type == "doctype")
            then (
              "${indent}  externalID: ${toJSON node.externalID}\n" +
              "${indent}  intSubset: ${toJSON node.intSubset}\n"
            )
            else ""
          )
          +
          (
            if (node ? attributes) then
            (
              let
                printAttribute = name: value: ''${indent}    ${name}: ${toJSON value}'' + "\n";
                a = (lib.concatStrings (lib.mapAttrsToList printAttribute node.attributes));
              in
              if a == "" then "" else "${indent}  attributes:\n" + a
            )
            else ""
          )
          +
          (
            if (node ? children) then
            (
              let c = (lib.concatStrings (builtins.map (yamlOfNodeInner (indent + "    ")) node.children)); in
              if c == "" then "" else "${indent}  children:\n" + c
            )
            else ""
          )
        )
        else
        if (node.type == "text" || node.type == "comment" || node.type == "cdata")
        then "${indent}- ${node.type}: ${toJSON node.value}\n"
        else
        "${indent}- ${node.type}: ${toJSON node}\n"
      )
      else
      "${indent}- typeless node: ${toJSON node}\n"
    );
    in node: (yamlOfNodeInner "" node);
in
yamlOfNode
