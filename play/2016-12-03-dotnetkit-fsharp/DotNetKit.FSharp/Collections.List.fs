namespace DotNetKit.FSharp

[<RequireQualifiedAccess>]
module List =
  let cons (x: 'x) (xs: list<'x>): list<'x> =
    x :: xs
