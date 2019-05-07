import { SyntaxRoot, syntaxRootAlloc } from "./syntax_root"
import { GreenElement, greenNodeToChildren, greenNodeToTextLen } from "./green"
import { LazyNode, lazyNodeGetOrInit, lazyNodeFromSeed } from "./lazy_node"
import { TextUnit } from "./types"
import { exhaust } from "./util";

/**
 * red ツリーの子ノードから見た親ノードの情報
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
 * red ツリーのノード
 *
 * green ツリーのノード (親ノードを参照していない、ソースコードにおける範囲は相対的な大きさしか知らない) をラップしたもの。これは親ノードとの関係や、元のソースコードにおける絶対的な範囲を持つ
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

  const greenChildren = greenNodeToChildren(green)
  const children: LazyNode[] = []
  for (let i = 0; i < greenChildren.length; i++) {
    const g = greenChildren[i]
    const offset = startOffset
    startOffset += greenNodeToTextLen(g)
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

export const syntaxNodeFromGreenNode = (green: GreenElement): SyntaxNode => {
  const root : SyntaxRoot = {
    arena: [],
  }
  const lazyNode = syntaxRootAlloc(root)
  const syntaxNode = lazyNodeGetOrInit(lazyNode, () => syntaxNodeNewRoot(root, green))
  return syntaxNode
}
