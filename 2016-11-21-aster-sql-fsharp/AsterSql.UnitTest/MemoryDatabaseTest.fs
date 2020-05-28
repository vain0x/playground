namespace AsterSql.UnitTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open AsterSql.Core

module MemoryDatabaseTest =
  [<Sealed>]
  type PersonTable(schemaPath) as this =
    inherit Table((schemaPath: DatabaseSchemaPath) / "persons")

    member val Name = Column<string>(this.TablePath / "name")

    member val Age = Column<Long>(this.TablePath / "age")

  [<Sealed>]
  type PublicDatabaseSchema(databasePath) as this =
    inherit DatabaseSchema((databasePath: DatabasePath) / "public")

    member val Persons =
      PersonTable(this.Path)

  [<Sealed>]
  type TestDatabase() as this =
    inherit MemoryDatabase("TestDatabase")

    member val Public = PublicDatabaseSchema(this.Path)

  let database = TestDatabase()
  let p = database.Public.Persons

  let ``test Table.Columns`` =
    test {
      do!
        p.Columns
        |> Seq.map (fun c -> c.Name)
        |> Seq.sort
        |> Seq.toList
        |> assertEquals ["age"; "name"]
    }

  let ``test Entity.Insert`` =
    test {
      use entity = database.Connect()
      entity.Insert
        ( p
        , fun r ->
            p.Name.[r] <- Sql.String("Miku")
            p.Age.[r] <- Sql.Int(16L)
        ) |> ignore<Long>
      return ()
    }
