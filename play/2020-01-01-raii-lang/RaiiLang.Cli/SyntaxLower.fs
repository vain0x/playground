module rec RaiiLang.SyntaxLower

open RaiiLang.Helpers
open RaiiLang.Syntax

let tokenAsBin (token: Token) =
  match token with
  | EqualEqualToken ->
    Some AEqBin

  | PlusToken ->
    Some AAddBin

  | EqualToken ->
    Some AAssignBin

  | _ ->
    None

let nodeIsTerm (node: Node) =
  match node with
  | LiteralNode
  | NameNode
  | GroupNode
  | BlockNode
  | CallNode
  | BinNode ->
    true

  | _ ->
    false

let nodeIsStmt (node: Node) =
  match node with
  | ExprNode
  | LetNode
  | FnNode ->
    true

  | _ ->
    false

let nodeToFirstToken pred (node: NodeData) =
  node.Children |> Seq.tryPick (fun element ->
    match element with
    | TokenElement t when pred t.Token ->
      Some t

    | _ ->
      None
  )

let nodeToFirstNode pred (node: NodeData) =
  node.Children |> Seq.tryPick (fun element ->
    match element with
    | NodeElement n when pred n.Node ->
      Some n

    | _ ->
      None
  )

let nodeToFilterNode pred (node: NodeData) =
  node.Children |> Seq.choose (fun element ->
    match element with
    | NodeElement n when pred n.Node ->
      Some n

    | _ ->
      None
  )
  |> Seq.toList

let lowerArg (node: NodeData) =
  assert (node.Node = ArgNode)

  let name =
    node
    |> nodeToFirstNode ((=) NameNode)
    |> Option.map lowerName

  ARefArg (name, node)

let lowerLiteral (node: NodeData) =
  assert (node.Node = LiteralNode)

  AIntLiteral node

let lowerName (node: NodeData) =
  assert (node.Node = NameNode)
  let ident =
    node
    |> nodeToFirstToken ((=) IdentToken)
    |> Option.map (fun ident -> ident.Text)
    |> Option.orElseWith (fun () ->
      node
      |> nodeToFirstToken ((=) AssertToken)
      |> Option.map (fun _ -> "assert")
    )

  AName (ident, node)

let lowerGroup (node: NodeData) =
  assert (node.Node = GroupNode)

  let item =
    node
    |> nodeToFirstNode nodeIsTerm
    |> Option.map lowerTerm

  AGroupTerm (item, node)

let lowerBlock (node: NodeData) =
  assert (node.Node = BlockNode)

  let item =
    node
    |> nodeToFilterNode nodeIsStmt
    |> List.map lowerStmt

  ABlockTerm (item, node)

let lowerCall (node: NodeData) =
  assert (node.Node = CallNode)

  let cal =
    node
    |> nodeToFirstNode nodeIsTerm
    |> Option.map lowerTerm

  let args =
    node
    |> nodeToFilterNode ((=) ArgNode)
    |> List.map lowerArg

  ACallTerm (cal, args, node)

let lowerBin (node: NodeData) =
  assert (node.Node = BinNode)

  let asBin (t: TokenData) = tokenAsBin t.Token

  let bin =
    node
    |> nodeToFirstToken (tokenAsBin >> Option.isSome)
    |> Option.bind asBin

  let terms =
    node
    |> nodeToFilterNode nodeIsTerm
    |> List.map lowerTerm

  let first = terms |> List.tryItem 0
  let second = terms |> List.tryItem 1

  ABinTerm (bin, first, second, node)

let lowerTerm (node: NodeData) =
  assert (node.Node |> nodeIsTerm)

  match node.Node with
  | LiteralNode ->
    lowerLiteral node

  | NameNode ->
    lowerName node |> ANameTerm

  | GroupNode ->
    lowerGroup node

  | BlockNode ->
    lowerBlock node

  | CallNode ->
    lowerCall node

  | BinNode ->
    lowerBin node

  | _ ->
    failwith "NEVER: nodeIsTerm bug"

let lowerStmt (node: NodeData) =
  match node.Node with
  | ExprNode ->
    let term =
      node
      |> nodeToFirstNode nodeIsTerm
      |> Option.map lowerTerm

    ATermStmt (term, node)

  | LetNode ->
    let terms =
      node
      |> nodeToFilterNode nodeIsTerm
      |> List.map lowerTerm

    let first = terms |> List.tryItem 0
    let second = terms |> List.tryItem 1

    ALetStmt (first, second, node)

  | FnNode ->
    let name =
      node
      |> nodeToFirstNode ((=) NameNode)
      |> Option.map lowerName

    let args =
      node
      |> nodeToFilterNode ((=) ArgNode)
      |> List.map lowerArg

    let body =
      node
      |> nodeToFirstNode nodeIsStmt
      |> Option.map lowerStmt

    AFnStmt (name, args, body, node)

  | _ ->
    failwith "NEVER: nodeIsStmt bug"

let lower (node: NodeData) =
  lowerStmt node
