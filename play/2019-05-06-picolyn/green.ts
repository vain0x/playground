import { SyntaxKind } from "./syntax_kind"
import { exhaust } from "./util"

/// startNode したときの children の長さ
type ChildrenOffset = number

export type GreenNode =
  | {
    type: "leaf",
    kind: SyntaxKind,
    text: string,
  }
  | {
    type: "branch",
    kind: SyntaxKind,
    textLen: number,
    children: GreenNode[],
  }

const greenNodeNewBranch = (kind: SyntaxKind, children: GreenNode[]): GreenNode => {
  const textLen = children.reduce((sum, node) => sum + greenNodeToTextLen(node), 0)
  return {
    type: "branch",
    kind,
    textLen,
    children,
  }
}

export const greenNodeToTextLen = (green: GreenNode): number => {
  if (green.type === "leaf") {
    return green.text.length
  }
  if (green.type === "branch") {
    return green.children.reduce((sum, node) => sum + greenNodeToTextLen(node), 0)
  }
  throw exhaust(green)
}

export const greenNodeToChildren = (green: GreenNode) => {
  if (green.type === "leaf") {
    return []
  }
  if (green.type === "branch") {
    return green.children
  }
  throw exhaust(green)
}

export class GreenNodeBuilder {
  private readonly parents: Array<[SyntaxKind, ChildrenOffset]> = []
  private readonly children: GreenNode[] = []

  /// トークンノードを追加する
  readonly leaf = (kind: SyntaxKind, text: string) => {
    this.children.push({
      type: "leaf",
      kind,
      text,
    })
  }

  /// 内部ノードの構築を開始する
  /// finishNode するまでの間にあるノードは、これの子ノードになる
  readonly startNode = (kind: SyntaxKind) => {
    const offset = this.children.length
    this.parents.push([kind, offset])
  }

  /// 内部ノードの構築を終了する
  readonly finishNode = () => {
    const [kind, offset] = this.parents.pop()!

    const children: GreenNode[] = []
    for (let i = this.children.length - offset - 1; i >= 0; i--) {
      children.push(this.children.pop()!)
    }
    children.reverse()

    this.children.push(greenNodeNewBranch(kind, children))
  }

  readonly finish = () => this.children[0]
}
