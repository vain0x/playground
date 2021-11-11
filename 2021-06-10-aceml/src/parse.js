const fs = require("fs").promises
const path = require("path")
const peg = require("pegjs")

const VERSION = "0.1.0"
const HELP_TEXT = `
aceml v${VERSION}

DESCRIPTION
    Parse an aceml code read from standard input,
    write converted data or syntax tree to standard output.

OPTIONS
    --format html     To HTML
    --format full     To full syntax tree in JSON (pretty printed)
    --format compact  To full syntax tree in JSON (no whitespace)
    --format default  To syntax tree in JSON (pretty printed, truncated)`

const GRAMMAR_FILE = path.join(__dirname, "../src/aceml-grammar.txt")
const error = err => {
  console.error(err)
  process.exit(1)
}

const convertToHtml = root => {
  const leaf = tag => tag === "link" || tag === "meta" || tag === "input" // FIXME: complete

  const encode = s => s
    .replace("<", "&lt;")
    .replace(">", "&gt;")
    .replace("'", "&apos;")
    .replace("\"", "&quot;")
    .replace("&", "&amp;")

  let out = ""

  const onNode = node => {
    if (node == null) return

    if (typeof node === "string") {
      if (node.trim() === "") {
        out += "\n"
        return
      }
      out += encode(node)
      return
    }

    if (typeof node === "object") {
      switch (node.kind) {
        case "root": {
          for (const child of node.children ?? []) {
            if (typeof child === "string") {
              out += child
              continue
            }

            if (typeof child === "object") {
              onNode(child)
              continue
            }
            console.error("warn: unknown toplevel node", child)
          }
          return
        }
        case "element": {
          const tag = node.tag ?? "span"

          out += "<"
          out += tag

          for (const attr of node.attr ?? []) {
            if (attr == null) continue

            if (typeof attr === "string") {
              if (attr.trim() === "") continue

              out += " "
              out += encode(attr)
              continue
            }

            if (typeof attr === "object") {
              if (attr.tag == null || attr.tag === "") {
                console.error("warn: attribute may not contain untagged elements", attr)
                continue
              }

              if (attr.children?.every(child => child == null || typeof child === "string")) {
                out += " "
                out += attr.tag
                out += "=\""
                out += encode(attr.children.join(""))
                out += "\""
                continue
              }

              // FIXME: implement
              out += " "
              out += encode(JSON.stringify(node.children))
              continue
            }

            console.error("warn: unknown attribute", attr)
          }
          out += ">"
          if (leaf(tag)) return

          for (const child of node.children ?? []) {
            onNode(child)
          }

          out += "</"
          out += tag
          out += ">"
          return
        }
        case "quoted":
          out += encode(node.text)
          return

        case "bad":
          console.warn("bad character at", node.loc)
          return

        default:
          console.error("warn: unknown node kind", node)
      }
    }

    console.error("warn: unknown node", node)
  }

  onNode(root)
  return out
}

{
  (async () => {
    process.once("SIGINT", () => process.exit(1))
    process.once("SIGTERM", () => process.exit(1))

    if (process.argv.includes("-h")||process.argv.includes("--help")) {
      console.log(HELP_TEXT)
      return
    }

    if (process.argv.includes("-V")||process.argv.includes("--version")) {
      console.log(VERSION)
      return
    }

    const grammar = await fs.readFile(GRAMMAR_FILE, { encoding: "utf-8" })
    const parser = peg.generate(grammar)

    const onSuccess = root => {
      if (process.argv.includes("html")) {
        console.log(convertToHtml(root))
      } else if (process.argv.includes("full")) {
        process.stdout.write(JSON.stringify(root, undefined, 2))
      } else if (process.argv.includes("compact")) {
        process.stdout.write(JSON.stringify(root))
      } else {
        process.stdout.write(JSON.stringify(root,
          (key, value) => key === "loc" ? undefined :
            (key === "attr" && (value == null || value?.length === 0) ? undefined :
              (key === "children" && (value == null || value?.every(x => x == null || x === "")) ? undefined :
                value)),
          2,
        ) + "\n")
      }
    }

    let buf = ""
    const input = process.stdin

    input.on("readable", () => {
      try {
        while (input.readable) {
          const chunk = input.read()
          if (chunk == null) return

          // console.error("readable", buf.length)
          buf += chunk.toString()
        }
      } catch (err) {
        error(err)
      }
    })

    input.on("end", () => {
      // console.error("end", buf.length)
      let root
      try {
        root = parser.parse(buf)
      } catch (err) {
        error(err)
        return
      }
      onSuccess(root)
    })
  })().catch(error)
}
