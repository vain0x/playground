import { Event } from "./types"
import { GreenNodeBuilder, greenToChildren, GreenElement } from "./green"
import { exhaust } from "./util"
import { syntaxNodeFromGreenTree, SyntaxNode, syntaxNodeToKind, syntaxNodeToChildren } from "./syntax_node"
import { syntaxKindIsOperator } from "./syntax_kind";

type BinaryOperator = "plus" | "minus"

type AstKind = "root" | "literal" | "bin"

interface AstNode {
  kind: AstKind,
  node: SyntaxNode,
}

type Ast<K> = AstNode & { kind: K }

/**
 * 構文解析の結果から green ツリーを組み立てる。
 */
const build = (events: Event[]) => {
  const builder = new GreenNodeBuilder()
  for (const event of events) {
    if (event.type === "token") {
      builder.token(event.kind, event.text)
      continue
    }
    if (event.type === "start") {
      builder.startNode(event.kind)
      continue
    }
    if (event.type === "finish") {
      builder.finishNode()
      continue
    }
    throw exhaust(event)
  }
  return builder.finish()
}

const astRootFromSyntaxNode = (node: SyntaxNode): AstNode | null =>
  syntaxNodeToKind(node) === "root" ? { kind: "root", node } : null

const astRootToChild = (ast: AstNode): AstNode | null =>
  syntaxNodeToChildren(ast.node)
    .map(astExpFromSyntaxNode)[0]
  || null

const astExpFromSyntaxNode = (node: SyntaxNode): AstNode | null =>
  astLiteralFromSyntaxNode(node)
  || astBinFromSyntaxNode(node)

const astLiteralFromSyntaxNode = (node: SyntaxNode): AstNode | null =>
  syntaxNodeToKind(node) === "literal" ? { kind: "literal", node } : null

const astLiteralAsToken = (ast: AstNode): GreenElement | null =>
  greenToChildren(ast.node.green)[0] || null

const astBinFromSyntaxNode = (node: SyntaxNode): AstNode | null =>
  syntaxNodeToKind(node) === "bin" ? { kind: "bin", node } : null

const astBinToOperatorToken = (ast: AstNode): GreenElement | null =>
  greenToChildren(ast.node.green)
    .find(green => syntaxKindIsOperator(green.kind))
  || null

const astBinAsOp = (ast: AstNode): BinaryOperator | null => {
  const token = astBinToOperatorToken(ast)
  if (!token || token.type === "node") return null

  if (token.text === "+") {
    return "plus"
  }
  if (token.text === "-") {
    return "minus"
  }
  return null
}

const astBinToChildNodes = (ast: AstNode): AstNode[] =>
  syntaxNodeToChildren(ast.node)
    .map(astExpFromSyntaxNode)
    .filter(node => node !== null)
    .map(node => node!)

const evalAst = (ast: AstNode): number | null => {
  if (ast.kind === "root") {
    const child = astRootToChild(ast)
    if (!child) {
      return null
    }
    return evalAst(child)
  }
  if (ast.kind === "literal") {
    const child = astLiteralAsToken(ast)
    if (!child || child.type !== "token" || child.kind !== "int") {
      return null
    }
    return +child.text
  }
  if (ast.kind === "bin") {
    const children =
      astBinToChildNodes(ast)
        .map(evalAst)
        .filter(value => value !== null)
        .map(value => value!)
    if (children.length !== 2) {
      return null
    }

    const op = astBinAsOp(ast)
    if (op === null) {
      return null
    }

    if (op === "plus") {
      return children[0] + children[1]
    }
    if (op === "minus") {
      return children[0] + children[1]
    }
    throw exhaust(op)
  }
  throw exhaust(ast.kind)
}

const main = () => {
  // 2+3
  const events: Event[] = [
    { type: "start", kind: "root" },
    { type: "start", kind: "bin" },
    { type: "start", kind: "literal" },
    { type: "token", kind: "int", text: "2" },
    { type: "finish" },
    { type: "token", kind: "plus", text: "+" },
    { type: "start", kind: "literal" },
    { type: "token", kind: "int", text: "3" },
    { type: "finish" },
    { type: "finish" },
    { type: "finish" }
  ]
  const green = build(events)
  const root = syntaxNodeFromGreenTree(green)
  console.log(root)

  const ast = astRootFromSyntaxNode(root)
  console.log(ast !== null && evalAst(ast))
}

main()
