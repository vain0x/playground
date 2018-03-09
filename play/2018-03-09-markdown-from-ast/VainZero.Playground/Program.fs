namespace rec VainZero.Markdown.Ast
  open System

  [<RequireQualifiedAccess>]
  type Emphasis =
    | Asterisk
    | Underscore

  [<RequireQualifiedAccess>]
  type EmphasisLevel =
    | Italic
    | Bold

  type HeadingLevel =
    | HeadingLevel of int

  type CodeLanguage =
    | CodeLanguage of string

  [<RequireQualifiedAccess>]
  type ItemizeSymbol =
    | Plus
    | Hyphen
    | Asterisk

  [<RequireQualifiedAccess>]
  type Flow =
    | Text
      of string
    | Emphasized
      of Emphasis * EmphasisLevel * Flow
    | Anchor
      of Flow * Uri
    | Code
      of string
    | Stack
      of Flow[]

  [<RequireQualifiedAccess>]
  type Block =
    | Paragraph
      of Flow
    | Heading
      of HeadingLevel * Flow
    | Code
      of option<CodeLanguage> * string
    | Itemize
      of ItemizeSymbol * (option<Flow> * option<Block>)[]
    | Enumerate
      of Block[]
    | Table
    | Separator
    | Stack
      of Block[]

  type Document =
    | Document
      of Block

namespace VainZero.Markdown
  open System.IO
  open VainZero.Markdown.Ast

  module Helper =
    let stackF flows = Flow.Stack flows
    let t str = Flow.Text str
    let b flow = Flow.Emphasized (Emphasis.Asterisk, EmphasisLevel.Bold, flow)
    let i flow = Flow.Emphasized (Emphasis.Asterisk, EmphasisLevel.Italic, flow)
    let c code = Flow.Code code
    let a uri flow = Flow.Anchor(flow, uri)

    let stackB blocks = Block.Stack blocks

    let heading level flow =
      Block.Heading (HeadingLevel level, flow)
    let h1 flow = heading 1 flow
    let h2 flow = heading 2 flow
    let h3 flow = heading 3 flow
    let h4 flow = heading 4 flow
    let h5 flow = heading 5 flow
    let h6 flow = heading 6 flow

    let p str = Block.Paragraph str

    let ul items = Block.Itemize (ItemizeSymbol.Hyphen, items)
    let liF flow = (Some flow, None)
    let liB block = (None, Some block)
    let liM flow block = (Some flow, Some block)

    let preL lang code =
      Block.Code (Some lang, code)

    let pre code =
      Block.Code (None, code)

  type Printer =
    private {
      Writer: TextWriter
      IndentLevel: int
      Indent: string
    }

  module rec Printer =
    type P = Printer

    [<Literal>]
    let IndentUnit = "    "

    let create writer =
      {
        Writer = writer
        IndentLevel = 0
        Indent = ""
      }

    let addIndent p =
      { p with
          IndentLevel = p.IndentLevel + 1
          Indent = p.Indent + IndentUnit
      }

    let writer (p: Printer) = p.Writer

    let writeIndent (context: Printer) =
      if context.IndentLevel > 0 then
        context.Writer.Write(context.Indent)

    let writeLine context =
      (context |> writer).WriteLine()

    let writeChar (c: char) context =
      (context |> writer).Write(c)

    let writeShort (str: string) context =
      let str = str.Replace("\r", " ").Replace("\n", " ")
      (context |> writer).Write(str)

    let writeLong (str: string) (context: Printer) =
      let writer = context |> writer
      let lines = str.Replace("\r\n", "\n").Split([|'\n'|])
      for line in lines do
        writeIndent context
        writer.WriteLine(line)

    let emphasisSymbol emphasis level =
      match (emphasis, level) with
      | (Emphasis.Underscore, EmphasisLevel.Italic) ->
        "_"
      | (Emphasis.Underscore, EmphasisLevel.Bold) ->
        "__"
      | (Emphasis.Asterisk, EmphasisLevel.Italic) ->
        "*"
      | (Emphasis.Asterisk, EmphasisLevel.Bold) ->
        "**"

    let itemizeSymbol =
      function
      | ItemizeSymbol.Plus -> '+'
      | ItemizeSymbol.Hyphen -> '-'
      | ItemizeSymbol.Asterisk -> '*'

    let headingSymbol (HeadingLevel level) =
      assert (1 <= level && level <= 6)
      System.String('#', level)

    let printFlow node (p: Printer) =
      match node with
      | Flow.Text text ->
        p |> writeShort text
      | Flow.Emphasized (emphasis, level, content) ->
        let symbol = emphasisSymbol emphasis level
        p |> writeShort symbol
        p |> printFlow content
        p |> writeShort symbol
      | Flow.Anchor (content, uri) ->
        p |> writeChar '['
        p |> printFlow content
        p |> writeChar ']'
        p |> writeChar '('
        p |> writeShort (uri |> string)
        p |> writeChar ')'
      | Flow.Code code ->
        p |> writeShort "``"
        p |> writeShort code
        p |> writeShort "``"
      | Flow.Stack flows ->
        for (i, flow) in flows |> Seq.indexed do
          if i > 0 then
            p |> writeChar ' '
          p |> printFlow flow

    let printBlock node (p: P) =
      match node with
      | Block.Paragraph content ->
        p |> writeIndent
        p |> printFlow content
        p |> writeLine
      | Block.Heading (level, content) ->
        p |> writeIndent
        p |> writeShort (headingSymbol level)
        p |> writeChar ' '
        p |> printFlow content
        p |> writeLine
      | Block.Code (lang, code) ->
        p |> writeIndent
        p |> writeShort "```"
        match lang with
        | Some (CodeLanguage lang) -> p |> writeShort lang
        | None -> ()
        p |> writeLine
        p |> writeLong (code.TrimEnd([|'\r'; '\n'|]))
        p |> writeIndent
        p |> writeShort "```"
        p |> writeLine
      | Block.Enumerate _ ->
        failwith "not impl"
      | Block.Itemize (symbol, items) ->
        let ch = itemizeSymbol symbol
        for (flow, item) in items do
          p |> writeIndent
          p |> writeChar ch
          match flow with
          | None -> ()
          | Some flow ->
            p |> writeChar ' '
            p |> printFlow flow
          p |> writeLine
          match item with
          | None -> ()
          | Some item ->
            (p |> addIndent) |> printBlock item
      | Block.Table ->
        failwith "not impl"
      | Block.Separator ->
        p |> writeIndent
        p |> writeShort "--------"
        p |> writeLine
      | Block.Stack blocks ->
        for (i, block) in blocks |> Seq.indexed do
          if i > 0 then
            p |> writeLine
          p |> printBlock block

    let printDocument (writer: TextWriter) (doc: Document) =
      let printer = create writer
      let (Document block) = doc
      printer |> printBlock block

namespace VainZero.Playground
  open System.IO
  open VainZero.Markdown
  open VainZero.Markdown.Ast

  module Program =
    open VainZero.Markdown.Helper

    [<EntryPoint>]
    let main _ =
      let cpp = CodeLanguage "cpp"
      let iostreamCode = """// C++ Hell World (using iostream)
#include <iostream>

void main() {
    std::cout << "Hello, world!" << std::endl;
}"""
      let printfCode = """// C++ Hello World (using printf)
#include <cstdio>

void main() {
    std::printf("Hello, world!\n");
}
"""

      let document =
        Document <|
          stackB [|
            h1 <| t "Hello world!"
            p <| t "Sample codes."
            ul [|
              liF <|
                stackF [|
                  t "There are"
                  i (t "two")
                  t "version of typical"
                  b (t "C++")
                  t "hello worlds."
                |]
              liM (t "iostream ver:") <|
                preL cpp iostreamCode
              liB <| ul
                [|
                  liF <| t "printf ver:"
                  liB <| preL cpp printfCode
                |]
            |]
          |]

      let markdownString =
        use writer = new StringWriter() :> TextWriter
        Printer.printDocument writer document
        writer.ToString()

      printf "%s" markdownString
      0
