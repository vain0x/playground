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

  let ``test Table.Insert`` =
    use entity = database.Connect()
    let p = database.Public.Persons
    test {
      do! p.TablePath.TableName |> assertEquals "persons"
      return ()
    }
