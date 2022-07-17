module Program

module TMap = AaTree.TMap

let private make n (s: string) =
  s.ToCharArray()
  |> Seq.map string
  |> Seq.sortBy id
  |> Seq.distinct
  |> Seq.mapi (fun i k -> i, k)
  |> Seq.sortBy (fun (_, v) -> s.IndexOf(v))
  |> Seq.map (fun (i, v) -> n + i * 10, v)
  |> Seq.toList
  |> TMap.ofList compare

[<EntryPoint>]
let main _ =
  let map1 = make 0 "ABCDEF"
  TMap.dump map1

  let map2 =
    let s =
      "quick brown fox jumps over the lazy dog"
        .Replace(" ", "")

    make 0 s

  TMap.dump map2

  let mutable m = map2

  for k, _ in TMap.toList map2 do
    let opt, m2 = TMap.remove k m
    eprintfn "remove(%d): %A" k opt
    TMap.dump m2
    m <- m2

  // let l, r = TMap.split 60 map2

  // TMap.dump l
  // TMap.dump r



  // for r in 1..4 do
  //   let n = 1 <<< r
  //   printfn "r = %d, n = %d" r n

  //   let m1 = make 0 (String.replicate n "ABCDEFG")
  //   // printfn "m1:"
  //   // TMap.dump display m1

  //   let m2 = make 1 (String.replicate n "PQRSTU")
  //   // printfn "m2:"
  //   // TMap.dump display m2

  //   // printfn "m1++m2:"
  //   // TMap.dump display (TMap.append m1 m2)
  //   let x = TMap.append m1 m2

  //   printStats ()

  // for i in -1 .. 5 do
  //   printfn "find %d -> %A" i (TMap.tryFind i map)

  // let m = 115
  // let l, t, r = TMap.split m map
  // printfn "split %d -> %A" m t
  // printfn "left:"
  // TMap.dump display l
  // printfn "right:"
  // TMap.dump display r

  // let map = TMap.ofList compare (keys |> List.mapi (fun i k -> k, i))

  // eprintfn "base"
  // printfn "digraph {"
  // printfn "%s" (TMap.toDot "base" map)

  // let split k =
  //   eprintfn "split %d" k
  //   let l, r = TMap.split k map
  //   printfn "%s" (TMap.toDot ($"split({k}).l") l)
  //   printfn "%s" (TMap.toDot ($"split({k}).r") r)

  // split 1
  // split 2
  // split 4
  // split 8
  // split 9

  // printfn "}"
  0
