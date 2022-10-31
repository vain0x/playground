module Util

type internal Queue<'T> = System.Collections.Generic.Queue<'T>
type internal Stack<'T> = System.Collections.Generic.Stack<'T>
type internal HashSet<'T> = System.Collections.Generic.HashSet<'T>
type internal HashMap<'K, 'T> = System.Collections.Generic.Dictionary<'K, 'T>

let inline internal todo () = failwith "todo"
let inline internal unreachable () = failwith "unreachable"
let inline internal (|Unreachable|) _ = unreachable ()

let inline internal swap<'T> (x: 'T byref) (y: 'T byref) =
  let t = x
  x <- y
  y <- t

module internal HashMap =
  let inline internal findOr key alt (map: HashMap<_, _>) =
    match map.TryGetValue(key) with
    | true, it -> it
    | _ -> alt

  let inline internal tryFind key (map: HashMap<_, _>) =
    match map.TryGetValue(key) with
    | true, value -> Some value
    | _ -> None
