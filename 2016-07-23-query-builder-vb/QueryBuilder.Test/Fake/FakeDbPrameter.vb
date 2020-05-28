Imports System.Data.Common

Public Class FakeDbParameter
    Inherits DbParameter

    Public Overrides Property DbType As DbType
    Public Overrides Property Direction As ParameterDirection
    Public Overrides Property IsNullable As Boolean
    Public Overrides Property ParameterName As String
    Public Overrides Property Size As Integer
    Public Overrides Property SourceColumn As String
    Public Overrides Property SourceColumnNullMapping As Boolean

    Public Overrides Property Value As Object

    Public Overrides Sub ResetDbType()
        Me.DbType = DbType.Int32
    End Sub
End Class
