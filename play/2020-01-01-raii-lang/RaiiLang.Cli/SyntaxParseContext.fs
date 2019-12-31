module RaiiLang.SyntaxParseContext

open RaiiLang.Helpers
open RaiiLang.Syntax

type TokenIndex = int

type TokenList = ResizeArray<TokenFat>

let vecPop (a: ResizeArray<_>) =
  let i = a.Count - 1
  let item = a.[i]
  a.RemoveAt(i)
  item

type ParseContext(tokens: TokenList) =
  let mutable currentIndex = 0

  let mutable stack = ResizeArray<NodeData>()

  do
    stack.Add({
      Node = SemiNode
      Children = ResizeArray()
    })

  let top () =
    stack.[stack.Count - 1]

  let assertInvariants () =
    assert (currentIndex <= tokens.Count)
    assert (stack.Count >= 1)

  let doBump () =
    assert (currentIndex + 1 <= tokens.Count)

    top () |> nodeAddTokenFat (tokens.[currentIndex])
    currentIndex <- currentIndex + 1
    assertInvariants ()

  let next () =
    tokens.[currentIndex].Token.Token

  let popNode node =
    let node = { top () with Node = node }
    stack.RemoveAt(stack.Count - 1)
    node

  let startNode () =
    stack.Add({
      Node = SemiNode
      Children = ResizeArray()
    })

  let startNodeWithPrevious () =
    let child = (top ()).Children |> vecPop
    startNode ()
    let parent = top ()
    parent.Children.Add(child)

    assertInvariants ()

  let endNode node =
    let child = popNode node
    let parent = top ()
    parent.Children.Add(NodeElement child)

    assertInvariants ()

  member _.AtEof =
    next () = EofToken

  member _.Next =
    next ()

  member _.Bump() =
    assert (currentIndex + 1 < tokens.Count)
    doBump ()

  member _.Eat(token: Token) =
    if next () = token then
      doBump ()
      true
    else
      false

  member _.AddError(error: SyntaxError) =
    let node = top ()
    node.Children.Add(ErrorElement error)

  member _.StartNode() =
    startNode ()

  member _.StartNodeWithPrevious() =
    startNodeWithPrevious ()

  member _.EndNode(node: Node) =
    endNode node

  member _.Finish() =
    assert (currentIndex = tokens.Count - 1)
    assert (stack.Count = 1)
    assert ((top ()).Node = SemiNode)

    doBump ()
    popNode SemiNode
