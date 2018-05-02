module Util

    open System
    open System.Text
    open Basis.Core

    let inline konst x _ = x
    let inline konst_unit x () = x

    let inline curry2 f x y = f (x, y)
    let inline uncurry2 f (x, y) = f x y

    let tap f x = f x |> konst x

    module List =
        ///最初の値を最初の状態として、左から畳み込む。
        ///Returns None iff given list is empty.
        let fold1 f = function
            | [] -> None
            | (x :: xs) -> Some <| List.fold f x xs

    module Str =
        let ofCharArray (arr: char []) =
            StringBuilder()
            |> tap (fun sb -> arr |> Array.iter (sb.Append >> ignore))
            |> string

        let filter pred self =
            self |> Str.toCharArray |> Array.filter pred |> ofCharArray
