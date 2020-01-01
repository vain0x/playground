module RaiiLang.SyntaxTokenizeContext

open RaiiLang.Helpers
open RaiiLang.Syntax

type TokenizeContext(sourceCode: string) =
  let mutable currentIndex = 0

  let mutable lastIndex = 0

  let mutable leading = ResizeArray<TriviaData>()

  let mutable tokens = ResizeArray<TokenFat>()

  let assertInvariants () =
    assert (0 <= lastIndex)
    assert (lastIndex <= currentIndex)
    assert (currentIndex <= sourceCode.Length)

  let bumpMany (len: int) =
    assert (currentIndex + len <= sourceCode.Length)

    currentIndex <- currentIndex + len

    assertInvariants ()

  let isFollowedBy (text: string) =
    currentIndex + text.Length <= sourceCode.Length
    && sourceCode
      |> strSlice currentIndex (currentIndex + text.Length)
      |> (=) text

  let currentText () =
    sourceCode |> strSlice lastIndex currentIndex

  let addToken (t: TokenData) =
    let token = t.Token

    if token |> tokenIsTrailingTrivia
      && tokens.Count > 0
      && leading.Count = 0 then
      // 前のトークンの後続トリビアにする。
      tokens.[tokens.Count - 1].Trailing.Add(t)
    else if token |> tokenIsLeadingTrivia then
      // 次のトークンの先行トリビアにする。
      leading.Add(t)
    else
      tokens.Add({
        Token = t
        Leading = ResizeArray(leading)
        Trailing = ResizeArray()
      })
      leading.Clear()

  member _.CurrentState() =
    currentIndex

  member _.CurrentText() =
    currentText ()

  member _.AtEof =
    currentIndex >= sourceCode.Length

  member _.Next =
    if currentIndex >= sourceCode.Length then
      '\x00'
    else
      sourceCode.[currentIndex]

  member _.Bump() =
    bumpMany 1

  member _.Eat(text: string) =
    if isFollowedBy text then
      bumpMany text.Length
      true
    else
      false

  member _.Commit(token: Token) =
    let text = currentText ()

    addToken ({ Token = token; Text = text })
    lastIndex <- currentIndex

    assertInvariants ()

  member _.Finish() =
    assert (lastIndex = sourceCode.Length)

    addToken ({ Token = EofToken; Text = "" })

    assert (leading.Count = 0)
    assert (tokens.Count >= 1)

    tokens
