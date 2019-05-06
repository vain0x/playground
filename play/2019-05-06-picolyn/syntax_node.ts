import { RedNode, redNodeRootFromGreenNode } from "./red"
import { SyntaxRoot } from "./syntax_root"
import { GreenNode } from "green"

/** 具象構文木のノード */
export interface SyntaxNode {
  root: SyntaxRoot,
  red: RedNode,
}

const syntaxNodeFromGreenNode = (green: GreenNode): SyntaxNode => {
  const root: SyntaxRoot = {
    red: redNodeRootFromGreenNode(green),
  }
  return {
    root,
    red: root.red,
  }
}
