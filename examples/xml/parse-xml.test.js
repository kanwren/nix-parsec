// TODO use "nix repl" to make this faster -> nix-nrepl.js

const langs = {};
langs.input = "xml";
langs.output = "yaml";

const fs = require("fs");
const child_process = require("child_process");

function getParse(parse, print = null) {
  return function parseFn(input) {
    const text = [
      //"with import <nixpkgs> {};",
      "let",
      `  parse = ${parse};`,
      `  print = ${print ? print : "x: x"};`,
      `  input = ${JSON.stringify(input)};`,
      `  result = parse input;`,
      "in",
      'if result.type == "success"',
      `then print result.value`,
      `else result`,
    ].join("\n");
    //console.log("```nix\n" + text + "\n```");
    // TODO use "nix repl" to make this faster -> nix-nrepl.js
    const nix_instantiate = child_process.spawnSync(
      'nix-instantiate', [
        '--eval',
        '--json',
        '--strict',
        '--expr',
        text,
      ],
      {
        //stdio: 'inherit',
        encoding: 'utf8',
        //cwd: '/tmp/nix-eval-js/home/work', // must exist
        //env: Object.assign({}, process.env, {
          // $HOME must be owned by user
          // https://github.com/NixOS/nix/issues/6834
          //HOME: '/tmp/nix-eval-js/home',
        //}),
      }
    )
    let result = nix_instantiate.output[1] // .trim() // stdout
    if (result == '' && nix_instantiate.status != 0) {
      result = nix_instantiate.output[2] // .split("\n")[0] // stderr
      //console.dir({ result })
      /*
      if (result.startsWith('error: ')) {
        result = 'ERROR EvalError ' + result.slice('error: '.length)
      }
      */
    }
    let output = nix_instantiate.output[1];
    if (output != "") {
      try {
        output = JSON.parse(output);
      }
      catch (error) {
        console.error(`${error} trying to parse ${JSON.stringify(output)}`);
      }
    }
    const error = nix_instantiate.output[2].split("\n")[0]
    return {
      status: nix_instantiate.status,
      output,
      error,
    };
  }
}

function parseTests(file) {

  const markdownSource = fs.readFileSync(file, "utf8");
  const markdownLines = markdownSource.split("\n");

  let title = null;

  // parse sections
  const sections = [];
  let section = null;
  for (const line of markdownLines) {
    if (line.startsWith('# ')) {
      title = line.replace(/^# /, "")
    }
    else if (line.startsWith('## ')) {
      // start a new section
      sections.push([]);
      section = sections[sections.length - 1];
      section.push(line)
    }
    else if (section) {
      section.push(line)
    }
  }

  for (const sectionId in sections) {
    const section = sections[sectionId];
    //console.log("section:"); console.log(section.join("\n"))
    const title = section[0].replace(/^## /, "")
    // parse code blocks
    const blocks = [];
    const langs = [];
    let block = null;
    for (const line of section) {
      if (!block && line.startsWith('```')) {
        // start a new block
        blocks.push([]);
        langs.push(line.slice(3)); // example: "```yaml" -> lang = "yaml"
        block = blocks[blocks.length - 1];
        //block.push(line)
      }
      else if (block && line.startsWith('```')) {
        // end of block
        block = null
      }
      else if (block) {
        block.push(line);
      }
      // else: ignore line
    }

    for (const blockId in blocks) {
      const block = blocks[blockId];
      blocks[blockId] = {
        code: block.join("\n"),
        lang: langs[blockId],
      }
    }

    sections[sectionId] = { title, blocks }
  }

  return { title, sections };
}

function main() {
  const baseFile = __filename.replace(/\.js$/, "");
  const casesFile = baseFile + ".md";
  const casesFileNew = baseFile + ".new.md";
  const { title, sections } = parseTests(casesFile);
  console.log(`running tests: ${title}`);
  const parse = getParse(
    "(import ./parse-xml.nix).parseXml",
    "(import ./parse-xml.nix).printYaml",
  );
  const newTests = [];
  newTests.push(`# ${title}`);
  newTests.push("");
  let numFailed = 0;
  let numTests = 0;
  for (const sectionId in sections) {
    numTests++;
    const section = sections[sectionId];
    const { title, blocks } = section;
    newTests.push(`## ${title}`);
    newTests.push("");
    const [inputBlock, expectedBlock] = blocks;
    const input = inputBlock.code;
    //const expected = expectedBlock.code;
    const actualResult = parse(input);
    //const actual = actualResult.output || actualResult.error;
    //console.dir(actualResult)

    if (actualResult.status == 0) {
      if (
        typeof actualResult.output == "string" &&
        actualResult.output.trim() == expectedBlock?.code.trim()
      ) {
        console.log(`pass ${title}`);
      }
      else {
        console.log(`fail ${title}`);
        numFailed++;
        //console.dir({ title, inputBlock, expectedBlock, actualResult }); throw new Error("todo") // debug
      }
    }
    else {
      if (actualResult.error.trim() == expectedBlock?.code.trim()) {
        console.log(`pass ${title}`);
      }
      else {
        console.log(`fail ${title}`);
        numFailed++;
        //console.dir({ title, inputBlock, expectedBlock, actualResult }); throw new Error("todo") // debug
      }
    }

    newTests.push("```" + langs.input);
    newTests.push(input);
    newTests.push("```");
    newTests.push("");

    if (actualResult.status == 0) {
      newTests.push("```" + langs.output);
      newTests.push(actualResult.output.replace(/\n$/s, "")); // remove trailing whitespace
      newTests.push("```");
      newTests.push("");
    }
    else {
      newTests.push("```");
      newTests.push(actualResult.error);
      newTests.push("```");
      newTests.push("");
    }

    //break; // debug
  }

  if (numFailed == 0) {
    console.log(`all pass`);
  }
  if (numFailed > 0) {
    console.log(`${numFailed} of ${numTests} failed`);
    console.log(`writing new test file: ${casesFileNew}`)
    console.log(`writing new test file: ${casesFileNew}`)
    fs.writeFileSync(casesFileNew, newTests.join("\n") + "\n", "utf8");
    console.log(`compare:`)
    console.log(`diff -u --color=always ${baseFile}{,.new}.md | less`)
    console.log(`update:`)
    console.log(`mv -v ${baseFile}{.new,}.md`)
  }
}

main();
