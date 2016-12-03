namespace DotNetKit.FSharp

[<RequireQualifiedAccess>]
module Option =
  let getOr x =
    function
    | Some x -> x
    | None -> x

  let getOrElse getX =
    function
    | Some x -> x
    | None -> getX ()

  let tryApply f x =
    try
      f x |> Some
    with
    | _ ->
      None
