import { TextUnit } from "./types"
import { GreenElement } from "./green"
import { SyntaxNode } from "./syntax_node"

type Lazy<U, V> =
  | {
    type: "uninit",
    value: U,
  }
  | {
    type: "inited",
    value: V,
  }

interface UninitSyntaxNode {
  offset: TextUnit,
  greenIndex: number,
}

export interface LazyNode {
  cell: Lazy<UninitSyntaxNode, SyntaxNode>,
}

type LazyNodeInitFun = (offset: TextUnit, greenIndex: number) => SyntaxNode

export const lazyNodeFromSeed = (offset: TextUnit, greenIndex: number): LazyNode => ({
  cell: {
    type: "uninit",
    value: {
      offset,
      greenIndex,
    },
  }
})

export const lazyNodeGetOrInit = (lazy: LazyNode, init: LazyNodeInitFun) => {
  if (lazy.cell.type === "uninit") {
    const { offset, greenIndex } = lazy.cell.value
    lazy.cell = {
      type: "inited",
      value: init(offset, greenIndex),
    }
  }
  return lazy.cell.value
}
