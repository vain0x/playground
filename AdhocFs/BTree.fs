namespace AdhocFs

module BTree =
  type BTree<'k, 'v> =
    | BTree

  let empty = BTree

  let length _ = 0

  let tryFind k this = None

  let add k v this = ()

  let remove k this = ()

  let toList this = []
