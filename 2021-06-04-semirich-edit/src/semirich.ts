import * as monaco from "monaco-editor"

type S = "PROGRAM" | "TAG" | "BODY"

class MyState implements monaco.languages.IState {
  constructor(
    public readonly stack: S[] = ["PROGRAM"],
  ) { }

  clone(): MyState {
    return new MyState(this.stack.slice())
  }

  equals(other: monaco.languages.IState): boolean {
    if (!(other instanceof MyState)) return false

    return this.stack.length === other.stack.length
      && this.stack.every((value, i) => value === other.stack[i])
  }
}

export const getInitialState = (): monaco.languages.IState => new MyState()

const REGEXP = /(\\?[ \t\r\n]+)|(%%[^\r\n]*)|(\{)|(\})|(:)|(\\[^ \t\r\n])|([^ \t\r\n{}:%\\]+|%)/g

export const lineTokenize = (line: string, state: monaco.languages.IState): monaco.languages.ILineTokens => {
  if (!(state instanceof MyState)) {
    console.error("state type error", state)
    return { tokens: [], endState: state }
  }

  const tokens: monaco.languages.IToken[] = []
  const push = (scopes: string) => tokens.push({ startIndex: i, scopes })

  const stack = state.stack.slice()
  let s = state.stack[state.stack.length - 1]!
  let i = 0
  let len = 0

  const r = new RegExp(REGEXP, "g")
  // console.log("tokenize", line, s)
  while (true) {
    i += len

    const m = r.exec(line)
    if (m == null) break

    const [, blank, comment, leftBrace, rightBrace, colon, escaped, verbatim] = m
    len = m[0].length
    // console.log(...m.slice(1), "i=", i, "len=", len)

    if (blank) {
      push("comment.spaces.backslash")
      continue
    }
    if (comment) {
      push("comment.line.double-percent")
      continue
    }
    if (leftBrace) {
      push("keyword.symbol")
      stack.push(s)
      s = "TAG"
      continue
    }
    if (colon) {
      push("keyword.symbol")
      s = "BODY"
      continue
    }
    if (rightBrace) {
      push("keyword.symbol")
      s = stack.length >= 2 ? stack.pop()! : "PROGRAM"
      continue
    }
    if (escaped) {
      push("constant.character.escape")
      continue
    }
    if (verbatim && s === "TAG") {
      push("keyword.tag")
      continue
    }
    if (verbatim && s === "BODY") {
      push("constant.character.verbatim")
      continue
    }
    if (verbatim && s === "PROGRAM") {
      push("invalid.illegal")
      continue
    }

    // console.log("no matching", line, i, s)
    break
  }

  // console.log(" --> ", tokens)
  return {
    tokens,
    endState: new MyState(stack),
  }
}

interface Pos {
  row: number
  column: number
}

interface TextRange {
  start: Pos
  end: Pos
}

type TokenKind =
  | "T_NEWLINES"
  | "T_BLANK"
  | "T_COMMENT"
  | "T_STRING"
  | "{"
  | "}"
  | ":"

type NodeKind =
  | "N_ROOT"
  | "N_ELEMENT"

interface Token {
  kind: TokenKind
  text: string
  range: TextRange
}

interface PElement {
  kind: NodeKind
  children: PNode[]
  range: TextRange
}

type PNode = Token | PElement

interface PTree {
  root: PElement & { kind: "N_ROOT" }
}

export const parse = (text: string): PTree => {
  const root: PElement & { kind: "N_ROOT" } = {
    kind: "N_ROOT",
    children: [],
    range: null!,
  }

  const stack: PElement[] = [root]

  let s: PElement = root
  let row = 0
  let line = ""
  let i = 0
  let len = 0

  const addToken = (kind: TokenKind) => {
    s.children.push({
      kind,
      text: line.slice(i, i + len),
      range: { start: { row, column: i }, end: { row, column: i + len } },
    })
  }

  for (const [localRow, localLine] of text.split(/\r?\n/g).entries()) {
    row = localRow
    line = localLine
    i = 0
    len = 0

    const r = new RegExp(REGEXP, "g")
    while (true) {
      i += len

      const m = r.exec(text)
      if (m == null) break

      const [, blank, comment, leftBrace, rightBrace, colon, escaped, verbatim] = m
      len = m[0].length

      if (blank || comment) continue

      if (colon) {
        addToken(":")
        continue
      }
      if (escaped || verbatim) {
        addToken("T_STRING")
        continue
      }
      if (leftBrace) {
        stack.push(s)
        s = { kind: "N_ELEMENT", children: [], range: null! }
        addToken("{")
        continue
      }
      if (rightBrace) {
        addToken("}")
        s = stack.pop() ?? root
        continue
      }
      throw new Error() // unreachable
    }
  }

  const go = (node: PNode, start: Pos): void => {
    if (node.kind === "N_ROOT" || node.kind === "N_ELEMENT") {
      let pos = start
      for (const child of node.children) {
        go(child, pos)
      }
      node.range = { start, end: pos }
      return
    }

    {
      const token = node as Token
      const { row, column } = start
      const len = token.text.length
      node.range = { start, end: { row, column: column + len } }
    }
  }

  return { root }
}

interface SemanticModel {
  emphasis: TextRange[]
}

const sema = (root: PElement): SemanticModel => {
  const emphasis: TextRange[] = []

  const go1 = (node: PNode): void => {
    if (node.kind === "N_ELEMENT" || node.kind === "N_ROOT") {
      const colon = node.children.findIndex(c => c.kind === ":")
      let tag: string | undefined

      for (const [i, child] of node.children.entries()) {
        if (i < colon && tag == null && child.kind === "T_STRING") {
          tag = child.text
          if (tag === "rule") {
            return goRule(node.children.slice(colon + 1))
          }
          continue
        }

        go1(child)
      }
    }
  }

  const goRule = (nodes: PNode[]): void => {
    const tokens: string[] = []

    for (const token of nodes) {
      if (token.kind === "T_STRING") {
        tokens.push(token.text)
      }
    }

    if (tokens[0] === "bold" && tokens[1] != null) {
      emphasis.push(JSON.parse(tokens[1]))
    }
  }

  go1(root)
  return { emphasis }
}

export const provideCodeActions: monaco.languages.CodeActionProvider["provideCodeActions"] = (model, range, _context, _token) => {
  const actions: monaco.languages.CodeAction[] = []

  const line = model.getLineContent(range.startLineNumber)
  const { tokens } = lineTokenize(line, getInitialState())

  for (const t of tokens) {
    if ((t.startIndex === range.startColumn - 1
      || t.startIndex === range.startColumn)
      && "{}:".includes(line[t.startIndex])
    ) {
      const row = range.startLineNumber - 1
      const column = t.startIndex
      actions.push({
        title: "太字",
        edit: {
          edits: [
            {
              resource: model.uri,
              edit: {
                range: {
                  startLineNumber: model.getLineCount(),
                  startColumn: 1,
                  endLineNumber: model.getLineCount(),
                  endColumn: 1,
                },
                text: `{rule:  }`
              }
            },
          ]
          id: "bold", title: "太字", arguments: [{ row, column }]
        }
      })
    }
  }
  return { actions, dispose: () => undefined }
}

export const registerSemirichLanguage = (m: typeof monaco) => {
  m.languages.register({
    id: "semirich",
    aliases: [
      "セミリッチエディット",
    ],
    extensions: [
      ".semirich",
    ],
  })

  m.languages.setTokensProvider("semirich", { getInitialState, tokenize: lineTokenize })
  m.languages.registerCodeActionProvider("semirich", { provideCodeActions })
}
