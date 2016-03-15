namespace Util

open System

module Console =
  // Too simple command line parser
  let ReadCommandLine argv =
    if argv |> Array.length > 0
    then argv
    else
      printfn "Input command line. Just use space-separated arguments:"
      let commandLine = Console.ReadLine()
      commandLine.Split ([|' '|], StringSplitOptions.RemoveEmptyEntries)

  let rec ReadYesNo () =
    match Console.ReadLine() with
    | "y" | "Y" -> true
    | "n" | "N" -> false
    | _ -> ReadYesNo ()

  /// 列のどの1つかをユーザに選択させる
  let inline AskWhichOne msg s =
    match s |> Seq.length with
    | 0 -> None
    | 1 -> Some (0, s |> Seq.head)
    | len ->
        s |> Seq.iteri (fun i e ->
            printfn "%0*d  %s" (Math.numDigits len) i (string e)
            )
        printfn "%s" msg

        let rec askLoop kont =
            printf "Input a number 0~%d, or '!' to skip: " (len - 1)
            let answer = Console.ReadLine()
            if answer = "!"
            then kont None
            else
              match Int32.TryParse answer with
              | true, n
                when 0 <= n && n < len
                -> kont <| Some n
              | _ ->
                  askLoop kont

        askLoop <| Option.map (fun i -> (i, s |> Seq.nth i))

  /// 入力を行区切りですべて取り出す
  let rec ReadLines() =
    Seq.unfold
      (fun () ->
        match Console.ReadLine() with
        | null -> None
        | s -> Some (s, ())
        ) ()
