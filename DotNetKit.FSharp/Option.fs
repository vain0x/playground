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

  let tryCatch f x =
    try
      f x
      None
    with
    | e ->
      Some e

  let ofTry =
    function
    | (true, x) ->
      Some x
    | (false, _) ->
      None
