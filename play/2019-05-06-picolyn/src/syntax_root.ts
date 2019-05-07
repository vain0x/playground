import { TextUnit } from "./types"
import { GreenElement } from "./green"
import { LazyNode, lazyNodeFromSeed } from "./lazy_node"

/**
 * 具象構文木の根ノード
 */
export interface SyntaxRoot {
  arena: LazyNode[],
}

export const syntaxRootAlloc = (root: SyntaxRoot): LazyNode => {
  const lazyNode = lazyNodeFromSeed(0, 0)
  root.arena.push(lazyNode)
  return lazyNode
}
