namespace AsterSql.Core

type DatabasePathRoot private () =
  static member val Instance = DatabasePathRoot()

  static member (/) (this: DatabasePathRoot, databaseName): DatabasePath =
    {
      DatabaseName =
        databaseName
    }

and DatabasePath =
  {
    DatabaseName:
      string
  }
with
  static member (/) (this: DatabasePath, schemaName): DatabaseSchemaPath =
    {
      DatabaseName =
        this.DatabaseName
      SchemaName =
        schemaName
    }

and DatabaseSchemaPath =
  {
    DatabaseName:
      string
    SchemaName:
      string
  }
with
  static member (/) (this: DatabaseSchemaPath, tableName): TablePath =
    {
      ParentPath =
        this |> Some
      TableName =
        tableName
    }

and TablePath =
  {
    ParentPath:
      option<DatabaseSchemaPath>
    TableName:
      string
  }
with
  static member (/) (this: TablePath, columnName): ColumnPath =
    {
      TablePath =
        this
      ColumnName =
        columnName
    }

and ColumnPath =
  {
    TablePath:
      TablePath
    ColumnName:
      string
  }
