''' <summary>
''' Represents a thing with or without an alias.
''' </summary>
Public Class OptionallyAliased
    Public ReadOnly Name As String
    Public ReadOnly AliasOrNull As String

    Public Sub New(name As String, aliasOrNull As String)
        Debug.Assert(name IsNot Nothing)
        Me.Name = name
        Me.AliasOrNull = aliasOrNull
    End Sub

    Public Sub New(name As String)
        Me.New(name, aliasOrNull:=Nothing)
    End Sub
End Class
