Public Class QueryBuilder
    Private ReadOnly _parameterFactory As IDbParameterFactory

    Public Sub New(parameterFactory As IDbParameterFactory)
        Me._parameterFactory = parameterFactory
    End Sub

    Public Function [And]() As ConditionBuilder
        Return New ConditionBuilder(ConditionCombinator.And, Me._parameterFactory)
    End Function

    Public Function [Or]() As ConditionBuilder
        Return New ConditionBuilder(ConditionCombinator.Or, Me._parameterFactory)
    End Function

    Public Function [Select]() As SelectBuilder
        Return New SelectBuilder(Me._parameterFactory)
    End Function
End Class
