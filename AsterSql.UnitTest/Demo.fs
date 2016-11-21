namespace AsterSql.UnitTest

open System
open AsterSql.Core

module Demo =
  type PersonTable =
    {
      Name
        : Column<string>
      Age
        : Column<int>
      Birthday
        : Column<DateTime>
    }

  type AppEntity() =
    member this.Persons =
      PersonTable()

  let demo () =
    let database = MemoryDatabase<AppEntity>()
    use entity = database.Connect()
    let () =
      let p = entity.Persons
      p.Add
        (fun r ->
          p.Name.[r] <- "Miku"
          p.Age.[r] <- 16L
          p.Birthday.[r] <- DateTimeOffset.Parse("2007/08/31")
        ) |> ignore
      p.Add
        (fun r ->
          p.Name.[r] <- "Yukari"
          p.Age.[r] <- 18L
          p.Birthday.[r] <- DateTimeOffset.Parse("2011/12/22")
        ) |> ignore
    let () =
      let records =
        entity.SelectTable<Person>()
      for r in records do
        printfn "%s (%d)" person.Name.[r] person.Age.[r]
    ()
