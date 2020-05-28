namespace FSharpKit.Misc.Disposables

open System

[<Sealed>]
type CountDisposable() =
  let count = ref 0

  member this.Count = !count
  member this.Dispose() = count |> incr

  interface IDisposable with
    override this.Dispose() = this.Dispose()
