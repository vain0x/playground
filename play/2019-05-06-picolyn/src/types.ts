import { SyntaxKind } from "./syntax_kind"

export type TextUnit = number

export type Event =
  | {
    type: "token",
    kind: SyntaxKind,
    text: string,
  }
  | {
    type: "start",
    kind: SyntaxKind,
  }
  | {
    type: "finish",
  }
