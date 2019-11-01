export type TokenKind =
  | "Eof"
  | "Error"
  | "Space"
  | "Eol"
  | "Ident"
  | "("
  | ")"
  | "["
  | "]"
  | "{"
  | "}"
  | ":"
  | ","
  | ";"
  | "Rule"

export interface Token {
  kind: TokenKind
  text: string
  start: number
  end: number
}

export type Ast =
  | { kind: "Term", name: string, children: Ast[] }
  | { kind: "RuleStmt", term: Ast }
  | { kind: "RootDecl", stmts: Ast[] }

const keywordToKind = (text: string): TokenKind => {
  switch (text) {
    case "rule":
      return "Rule"
    default:
      return "Ident"
  }
}

class Parser {
  private _last = 0
  private _index = 0

  private _next: Token | null = null

  private _errors: string[] = []

  constructor(
    private readonly _text: string,
  ) {
  }

  errors() {
    return this._errors
  }

  locate(index: number) {
    index = Math.max(0, Math.min(index, this._text.length))

    let i = 0
    let row = 0
    let col = 0
    while (i < index) {
      if (this._text[i] === '\n') {
        row++
        col = 0
      } else {
        col++
      }
      i++
    }

    return [row, col]
  }

  private nextChar() {
    return this._text[this._index]
  }

  private bumpChar() {
    if (this._index < this._text.length) {
      this._index++
    }
  }

  private ate() {
    return this._last < this._index
  }

  private setNext(kind: TokenKind) {
    this._next = {
      kind,
      text: this._text.slice(this._last, this._index),
      start: this._last,
      end: this._index,
    }
    this._last = this._index
    return this._next
  }

  private nextToken(): Token {
    if (this._next !== null) {
      return this._next
    }

    if (this._index >= this._text.length) {
      return {
        kind: "Eof",
        text: "",
        start: this._index,
        end: this._index,
      }
    }

    while (" \t\r".includes(this.nextChar())) {
      this.bumpChar()
    }
    if (this.ate()) {
      return this.setNext("Space")
    }

    if (this.nextChar() === '\n') {
      this.bumpChar()
      return this.setNext("Eol")
    }

    while (this.nextChar().match(/[$a-zA-Z0-9_]/)) {
      this.bumpChar()
    }
    if (this.ate()) {
      let token = this.setNext("Ident")
      token.kind = keywordToKind(token.text)
      return token
    }

    if ("[]{};,".includes(this.nextChar())) {
      const kind = this.nextChar() as TokenKind
      this.bumpChar()
      return this.setNext(kind)
    }

    this.bumpChar()
    this.setNext("Error")
    this.addError("Unknown char")
    this.bump()
    return this.nextToken()
  }

  nextRow(): number {
    return this.locate(this.nextToken().start)[0]
  }

  nextCol(): number {
    return this.locate(this.nextToken().start)[1]
  }

  next(): TokenKind {
    return this.nextToken().kind
  }

  bump(): Token {
    const token = this.nextToken()
    this._next = null
    return token
  }

  addError(msg: string) {
    this._errors.push(msg)
  }
}

const tokenKindIsFirstOfTerm = (kind: TokenKind) =>
  kind === "Ident"
  || kind === "["
  || kind === "{"

const tokenKindIsFirstOfStmt = (kind: TokenKind) =>
  kind === "Rule"

const eatInlineSpaces = (p: Parser) => {
  while (p.next() === "Space") {
    p.bump()
  }
}

/// Returns true if EOL skipped.
const eatSpaces = (p: Parser) => {
  let eol = false

  while (true) {
    if (p.next() === "Space") {
      p.bump()
      continue
    }

    if (p.next() === "Eol") {
      eol = true
      p.bump()
      continue
    }

    break
  }

  return eol
}

const eatEof = (p: Parser) => {
  if (p.next() !== "Eof") {
    p.addError("Expected EOF")
  }
}

const eatComma = (p: Parser) => {
  if (eatSpaces(p)) {
    return true
  }

  if (p.next() === ",") {
    p.bump()
    return true
  }

  p.addError("Expected , or EOL")
  return false
}

const eatSemi = (p: Parser) => {
  if (eatSpaces(p)) {
    return
  }

  if (p.next() === ";") {
    p.bump()
    return
  }

  p.addError("Expected ; or EOL")
  return false
}

const parseTerm = (p: Parser): Ast => {
  let name = ""
  if (p.next() === "Ident") {
    name = p.bump().text
  }

  eatInlineSpaces(p)

  let children: Ast[] = []
  if (p.next() === "[") {
    p.bump()

    while (p.next() !== "]" && !tokenKindIsFirstOfStmt(p.next())) {
      eatSpaces(p)

      children.push(parseTerm(p))

      eatInlineSpaces(p)
      if (p.next() === "]") {
        break
      }

      eatComma(p)
    }

    if (p.next() === "]") {
      p.bump()
    } else {
      p.addError("Expected ]")
    }
  }

  return {
    kind: "Term",
    name,
    children,
  }
}

const parseStmt = (p: Parser): Ast | null => {
  switch (p.next()) {
    case "Rule":
      p.bump()
      eatInlineSpaces(p)

      if (p.next() !== "Ident") {
        p.addError("Expected an identifier")
      }

      const term = parseTerm(p)
      eatSemi(p)

      return {
        kind: "RuleStmt",
        term,
      }
    default:
      throw new Error("Expected 'rule'")
  }
}

const parseRoot = (p: Parser): Ast => {
  const stmts: Ast[] = []

  eatSpaces(p)

  while (true) {
    if (p.next() === "Eof") {
      break
    }

    if (!tokenKindIsFirstOfStmt(p.next())) {
      p.addError("Expected a statement")
      p.bump()

      while (
        p.next() !== "Eof"
        && !tokenKindIsFirstOfStmt(p.next())
        && p.nextCol() > 2
      ) {
        p.bump()
      }
      continue
    }

    const stmt = parseStmt(p)
    if (stmt !== null) {
      stmts.push(stmt)
    }
    eatSpaces(p)
  }

  eatEof(p)

  return {
    kind: "RootDecl",
    stmts,
  }
}

export const parse = (text: string): [Ast, string[]] => {
  const p = new Parser(text)
  const root = parseRoot(p)
  const errors = p.errors()
  return [root, errors]
}
