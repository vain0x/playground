Imports System.Text

Public Class SqlEmitter
    Private _sql As New StringBuilder()

    Protected Sub AppendLine([string] As String)
        Me._sql.AppendLine([string])
    End Sub

    Protected Sub AppendConcat(Of X)(
            conjunction As String,
            separator As String,
            xs As IEnumerable(Of X),
            body As Action(Of X)
            )
        If xs Is Nothing Then Return
        Dim isFirst = True
        For Each element In xs
            If isFirst Then
                Me.AppendLine(conjunction)
                isFirst = False
            Else
                Me.AppendLine(separator)
            End If

            body(element)
        Next
    End Sub

    Protected Sub AppendAlias(name As OptionallyAliased)
        Me.AppendLine(name.Name)
        If name.AliasOrNull IsNot Nothing Then
            Me.AppendLine("as")
            Me.AppendLine(name.AliasOrNull)
        End If
    End Sub

    Protected Sub AppendConditions(conjunction As String, condition As ConditionBuilder)
        If condition Is Nothing Then Return
        Me.AppendLine(conjunction)
        Me.AppendLine(condition.ToString())
    End Sub

    Public Overrides Function ToString() As String
        Return Me._sql.ToString()
    End Function
End Class
