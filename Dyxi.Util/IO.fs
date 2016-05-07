namespace Dyxi.Util

open System
open System.IO

module DayOfWeek =
  let all = Enum.getValues<DayOfWeek>

  let kanjis = [|"“ú"; "ŒŽ"; "‰Î"; "…"; "–Ø"; "‹à"; "“y"|]

  let toKanji (dow: DayOfWeek) =
    kanjis.[int dow]

  let ofKanji k =
    kanjis
    |> Array.tryFindIndex ((=) k)
    |> Option.map (fun i -> all.[i])

module DateTime =
  let theLatestSunday (date: DateTime) =
    date.AddDays(- (date.DayOfWeek |> float))

  let weekDays date =
    let sunday = date |> theLatestSunday
    in [ for i in 0..6 -> sunday.AddDays(float i) ]

module Stream =
  let readToEndAsync (stream: Stream) =
    let reader = new StreamReader(stream)
    in reader.ReadToEndAsync() |> Async.AwaitTask

module Random =
  let element (xs: seq<'x>) (rng: Random): option<'x> =
    let len = xs |> Seq.length
    in
      if len = 0
      then None
      else xs |> Seq.item (rng.Next(0, len - 1)) |> Some
