namespace AsterSql.UnitTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open AsterSql.Core

module MemoryDatabaseTest =
  [<Sealed>]
  type PersonTable private (tablePath) =
    inherit MemoryTable(tablePath)

    new(schemaPath) =
      PersonTable((schemaPath: DatabaseSchemaPath) / "persons")

    member val Name = MemoryColumn<string>(tablePath / "name")

    member val Age = MemoryColumn<Long>(tablePath / "age")

  [<Sealed>]
  type TestEntity(schemaPath) =
    inherit Entity()

    member val Persons =
      PersonTable(schemaPath)

    override this.Dispose() =
      ()

  let database = MemoryDatabase("database")

  let ``test Table.Insert`` =
    let schema = database.GetSchema<TestEntity>("public")
    use entity = schema.Connect()
    let p = entity.Persons
    ()
