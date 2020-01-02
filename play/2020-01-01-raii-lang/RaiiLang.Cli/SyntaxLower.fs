module rec RaiiLang.SyntaxLower

open RaiiLang.Helpers
open RaiiLang.Syntax

let tokenToText (token: TokenData) =
  token.Text

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

let tokenAsCallBy (token: Token) =
  match token with
  | RefToken ->
    Some ByRef

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
  | ExternFnNode
  | FnNode
  | SemiNode ->
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

let lowerParam (node: NodeData) =
  assert (node.Node = ParamNode)

  let callBy =
    node
    |> nodeToFirstToken (tokenAsCallBy >> Option.isSome)
    |> Option.bind (fun token -> tokenAsCallBy token.Token)
    |> Option.defaultValue ByMove

  let term =
    node
    |> nodeToFirstNode nodeIsTerm
    |> Option.map lowerTerm

  AArg (callBy, term, node)

let lowerLiteral (node: NodeData) =
  assert (node.Node = LiteralNode)

  let intToken =
    node
    |> nodeToFirstToken ((=) IntToken)
    |> Option.map tokenToText

  AIntLiteral (intToken, node)

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
    |> nodeToFilterNode ((=) ParamNode)
    |> List.map lowerParam

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
  assert (node.Node |> nodeIsStmt)

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

  | ExternFnNode ->
    let name =
      node
      |> nodeToFirstNode ((=) NameNode)
      |> Option.map lowerName

    let args =
      node
      |> nodeToFilterNode ((=) ParamNode)
      |> List.map lowerParam

    AExternFnStmt (name, args, node)

  | FnNode ->
    let name =
      node
      |> nodeToFirstNode ((=) NameNode)
      |> Option.map lowerName

    let args =
      node
      |> nodeToFilterNode ((=) ParamNode)
      |> List.map lowerParam

    let body =
      node
      |> nodeToFirstNode ((=) BlockNode)
      |> Option.map lowerTerm

    AFnStmt (name, args, body, node)

  | SemiNode ->
    let stmts =
      node
      |> nodeToFilterNode nodeIsStmt
      |> List.map lowerStmt

    ASemiStmt (stmts, node)

  | _ ->
    failwith "NEVER: nodeIsStmt bug"

let lower (node: NodeData) =
  assert (node.Node = SemiNode)

  lowerStmt node
