Imports System.Data.Common

Public Class FakeDbParameterFactory
    Implements IDbParameterFactory

    Public Function Create(name As String, value As Object) As DbParameter Implements IDbParameterFactory.Create
        Return New FakeDbParameter() With {
            .DbType = DbType.Int32,
            .ParameterName = name,
            .Value = value
        }
    End Function
End Class
