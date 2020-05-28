/**
 * 具象構文木のノードの種類 (トークンの種類、または式の種類)
 */
export type SyntaxKind =
  | "error"
  | "whitespace"
  | "int"
  | "plus"
  | "minus"
  | "root"
  | "bin"
  | "literal"

export const syntaxKindIsOperator = (syntaxKind: SyntaxKind) =>
  syntaxKind === "plus"
  || syntaxKind === "minus"
