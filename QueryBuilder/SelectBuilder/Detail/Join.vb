Namespace Detail
    Public Class [Join]
        ' inner join, outer join, etc.
        Public ReadOnly JoinType As String
        Public ReadOnly TableName As Name
        Public ReadOnly OnExpression As String

        Public Sub New(joinType As String, tableName As Name, [on] As String)
            Me.JoinType = joinType
            Me.TableName = tableName
            Me.OnExpression = [on]
        End Sub
    End Class
End Namespace
