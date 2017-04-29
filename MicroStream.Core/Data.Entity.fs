namespace MicroStream.Data.Entity

open System.Data.Entity

module DbContext =
  let saveAsync (context: DbContext) =
    async {
      let! cancellationToken = Async.CancellationToken
      let! _ = context.SaveChangesAsync(cancellationToken) |> Async.AwaitTask
      return ()
    }

module Database =
  let connect (database: IDatabase) =
    database.Connect()

  let generateIdAsync (database: IDatabase) =
    async {
      use context = database.Connect()
      let sequence = Sequence()
      context.Set<Sequence>().Add(sequence) |> ignore
      do! context |> DbContext.saveAsync
      return sequence.Id
    }
