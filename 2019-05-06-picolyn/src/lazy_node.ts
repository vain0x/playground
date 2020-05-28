import { TextUnit } from "./types"
import { GreenElement } from "./green"
import { SyntaxNode } from "./syntax_node"

/**
 * 生成が遅延されているデータを表す。
 */
type Lazy<U, V> =
  | {
    type: "uninit",
    /** 生成されるオブジェクトの元になるデータ */
    value: U,
  }
  | {
    type: "inited",
    /** 生成済みのデータ */
    value: V,
  }

/**
 * 生成が遅延されたままのノードが持つデータ
 */
interface UninitSyntaxNode {
  offset: TextUnit,
  greenIndex: number,
}

export interface LazyNode {
  cell: Lazy<UninitSyntaxNode, SyntaxNode>,
}

type LazyNodeInitFun = (offset: TextUnit, greenIndex: number) => SyntaxNode

/**
 * 具象構文木のノードを生成する。構築は必要になるまで遅延される。
 */
export const lazyNodeFromSeed = (offset: TextUnit, greenIndex: number): LazyNode => ({
  cell: {
    type: "uninit",
    value: {
      offset,
      greenIndex,
    },
  }
})

/**
 * 構築が遅延されているノードを取り出す。
 *
 * 構築済みでなければ構築する。
 */
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
