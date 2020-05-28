import { TextUnit } from "./types"
import { GreenElement } from "./green"
import { LazyNode, lazyNodeFromSeed } from "./lazy_node"

/**
 * 具象構文木の根ノード。
 *
 * rowan ではメモリ管理のための機能やユーザーデータを有していたが、いまは特に役割がない。
 */
export interface SyntaxRoot {
  type: "root",
}
