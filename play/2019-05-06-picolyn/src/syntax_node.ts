import { SyntaxRoot } from "./syntax_root"
import { GreenElement, greenToChildren, greenToTextLen } from "./green"
import { LazyNode, lazyNodeGetOrInit, lazyNodeFromSeed } from "./lazy_node"
import { TextUnit } from "./types"
import { exhaust } from "./util";

/**
 * 具象構文木の子ノードから見た親ノードの情報
 */
interface ParentData {
  /** 親ノード */
  parent: SyntaxNode,
  /** 親ノードがテキスト上で占める範囲の開始位置 */
  startOffset: TextUnit,
  /** 親ノードの何番目の子ノードであるか */
  indexInParent: number,
}

/**
 * 具象構文木のノード
 *
 * Green ツリーのノード (親ノードを参照していない、ソースコードにおける範囲は相対的な大きさしか知らない) をラップしたもの。
 * これは親ノードとの関係や、元のソースコードにおける絶対的な範囲を持つ
 */
export interface SyntaxNode {
  root: SyntaxRoot,
  parent: ParentData | null,
  green: GreenElement,
  children: LazyNode[],
}

const parentToStartOffset = (parent: ParentData | null) =>
  parent ? parent.startOffset : 0

const syntaxNodeNew = (root: SyntaxRoot, parent: ParentData | null, green: GreenElement): SyntaxNode => {
  let startOffset = parentToStartOffset(parent)

  const greenChildren = greenToChildren(green)
  const children: LazyNode[] = []
  for (let i = 0; i < greenChildren.length; i++) {
    const g = greenChildren[i]
    const offset = startOffset
    startOffset += greenToTextLen(g)
    if (g.type === "token") {
      continue
    }
    if (g.type === "node") {
      children.push(lazyNodeFromSeed(offset, i))
      continue
    }
    throw exhaust(g)
  }

  return {
    root,
    parent,
    children,
    green,
  }
}

export const syntaxNodeNewRoot = (root: SyntaxRoot, green: GreenElement): SyntaxNode => {
  const NO_PARENT = null
  return syntaxNodeNew(root, NO_PARENT, green)
}

export const syntaxNodeFromGreenTree = (green: GreenElement): SyntaxNode => {
  const root : SyntaxRoot = { type: "root" }
  const lazyNode = lazyNodeFromSeed(0, 0)
  const syntaxNode = lazyNodeGetOrInit(lazyNode, () => syntaxNodeNewRoot(root, green))
  return syntaxNode
}
