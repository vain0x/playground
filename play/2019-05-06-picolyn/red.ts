import { GreenNode, greenNodeToChildren, greenNodeToTextLen } from "./green"

type TextUnit = number

/**
 * red ツリーの子ノードから見た親ノードの情報
 */
interface ParentData {
  /** 親ノード */
  parent: RedNode,
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
export interface RedNode {
  green: GreenNode,
  parent: ParentData | null,
  children: RedChild[],
}

/**
 * red ツリーの子ノード。
 *
 * red ツリーは必要に応じて生成される。生成された red ノードの子ノードは、はじめすべて zigot にしておき、子ノードが参照されるたびに child で置き換える。
 */
export type RedChild =
  | {
    type: "zigot",
    offset: TextUnit,
  }
  | {
    type: "child",
    child: RedNode,
  }

const redChildNewZigot = (offset: number): RedChild => ({
  type: "zigot",
  offset,
})

const parentToStartOffset = (parent: ParentData | null) =>
  parent ? parent.startOffset : 0

const redChildrenFromGreenNode = (green: GreenNode, parent: ParentData | null): RedChild[] => {
  let startOffset = parentToStartOffset(parent)
  return greenNodeToChildren(green)
    .map(green => {
      const offset = startOffset
      const textLen = greenNodeToTextLen(green)
      startOffset += textLen

      // NOTE: 孫世代以下はまだ生成しない
      return redChildNewZigot(offset)
    })
}

const redNodeFromGreenNode = (green: GreenNode, parent: ParentData | null): RedNode => {
  const children = redChildrenFromGreenNode(green, parent)
  return { green, parent, children }
}

export const redNodeRootFromGreenNode = (green: GreenNode): RedNode => {
  const NO_PARENT = null
  return redNodeFromGreenNode(green, NO_PARENT)
}
