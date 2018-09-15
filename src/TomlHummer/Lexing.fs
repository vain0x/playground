module TomlHummer.Lexing
  open System
  open System.Text.RegularExpressions
  open TomlHummer

  module RegexLexer =
    open System
    open System.Text.RegularExpressions

    exception UnknownTokenException of int

    [<RequireQualifiedAccess>]
    type Pattern<'T> =
      {
        Regex: Regex
        ToToken: Match -> option<'T>
      }

    let pattern regex toToken: Pattern<_> =
      {
        Regex = regex
        ToToken = toToken
      }

    let lex (table: #seq<Pattern<_>>) endToken (source: string) (start: int) (endIndex: int) =
      let withEndToken acc =
        match endToken with
        | Some t -> t :: acc
        | None -> acc
      let ps =
        [|
          for pattern in table ->
            pattern, pattern.Regex.Match(source, start, endIndex - start)
        |]
        |> Array.rev

      let rec go acc i =
        if i >= endIndex then
          (endToken :: acc) |> Seq.choose id |> Seq.rev |> Seq.toList
        else
          let mutable mpi = ps.Length
          for pi in 0..ps.Length - 1 do
            let mutable p, m = ps.[pi]
            if m.Success && m.Index < i then
              m <- m.NextMatch()
              ps.[pi] <- p, m
            if m.Success && m.Index = i && m.Index + m.Length <= endIndex then
              mpi <- pi
          if mpi = ps.Length then
            UnknownTokenException(i) |> raise
          else
            let p, m = ps.[mpi]
            let t = p.ToToken(m)
            ps.[mpi] <- p, m.NextMatch()
            go (t :: acc) (m.Index + m.Length)
      go [] start

  let private table: list<RegexLexer.Pattern<_>> =
    let p (s: string) f = RegexLexer.pattern (Regex(s)) f
    let q (s: string) t = RegexLexer.pattern (Regex(s)) (fun _ -> Some t)
    let skip s = p s (fun _ -> None)
    [
      // Spaces.
      skip """[ \t\r\n]+"""
      // Line comments
      skip """#[^\r\n]*\r?\n"""
      // Identifiers
      p """(?![0-9])[_A-Za-z0-9-]+"""
        (fun g -> g.Value |> TomlToken.Ident |> Some)
      // Integer
      p """(?!_)[-+0-9_]+"""
        (fun g -> g.Value |> int |> TomlToken.Int |> Some)
      // Rich string
      p ("\"" + """((?:\\[^"\r\n]|[^\\\r\n]+)*)""" + "\"")
        (fun g -> g.Groups.[1].Value |> TomlToken.String |> Some)
      // Punctuations
      q """\.""" TomlToken.Dot
      q "," TomlToken.Comma
      q "=" TomlToken.Eq
      q """\[\[""" TomlToken.BracketLL
      q """\]\]""" TomlToken.BracketRR
      q """\[""" TomlToken.BracketL
      q """\]""" TomlToken.BracketR
      q """\{""" TomlToken.BraceL
      q """\}""" TomlToken.BraceR
    ]

  let lex (source: string) (startIndex: int) (endIndex: int) =
    RegexLexer.lex table (Some TomlToken.Eof) source startIndex endIndex
