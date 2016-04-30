namespace Dyxi.Muse.Model

open FSharp.Data.Sql

type sql =
  SqlDataProvider<
      Common.DatabaseProviderTypes.MYSQL
    , ConnectionString  = "Data Source=localhost; user=root; password=;"
    , ResolutionPath    = @"C:\Program Files (x86)\MySQL\MySQL Connector Net 6.9.8\Assemblies\v4.0"
    , UseOptionTypes    = true
    , Owner             = "dyxi_muse"
    >

module Database =
  let ctx = sql.GetDataContext()

  let internal updatedEvent = Event<unit>()

  let updated = updatedEvent.Publish

  let update () =
    ctx.SubmitUpdates ()
    updatedEvent.Trigger()

[<AutoOpen>]
module DatabaseExtension =
  let db = Database.ctx.DyxiMuse

  // Login
  let loginUser =
    db.Users.Individuals.``1``
