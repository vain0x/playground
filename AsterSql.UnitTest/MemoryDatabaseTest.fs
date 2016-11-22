namespace AsterSql.UnitTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open AsterSql.Core

module MemoryDatabaseTest =
  type PersonTable() as this =
    inherit MemoryTable("persons")

    member val Name = MemoryColumn<string>(this, "name")

    member val Age = MemoryColumn<Long>(this, "age")

  type TestEntity(databaseName, schemaName) as this =
    inherit Entity()

    member val Persons =
      PersonTable(databaseName, schemaName, "persons")

    override this.Dispose() =
      ()

  let database = MemoryDatabase("database")

  let ``test Table.Insert`` =
    let schema = database.GetSchema<TestEntity>("public")
    use entity = schema.Connect()
    ()
