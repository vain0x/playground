Imports System.Data.Common

Public Interface IDbParameterFactory
    Function Create(name As String, value As Object) As DbParameter
End Interface
