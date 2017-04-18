namespace MicroStream.Data.Entity

open System
open System.Data.Entity
open System.Linq
open System.Linq.Expressions
open System.Runtime.CompilerServices

[<Extension>]
type QueryableExtensionForFSharp =
  [<Extension>]
  static member TryFirstAsync(q: IQueryable<_>) =
    async {
      let! cancellationToken = Async.CancellationToken
      let! var =
        QueryableExtension.FirstOrNullAsyncCore(q, cancellationToken) |> Async.AwaitTask
      return
        match var with
        | null -> None
        | var -> Some var.value
    }

  [<Extension>]
  static member TryFirstAsync(q: IQueryable<_>, predicate: Expression<Func<_, bool>>) =
    q.Where(predicate).TryFirstAsync()
