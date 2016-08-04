Public MustInherit Class ExpressionsCombinator
    Public MustOverride ReadOnly Property Unit As String
    Protected MustOverride ReadOnly Property Combinator As String

    Public Overridable Function Concat(expressions As IEnumerable(Of String)) As String
        Return If(expressions.Any(),
            String.Join(Me.Combinator,
                expressions.Where(Function(e) e <> Me.Unit) _
                .Select(Function(e) "(" + e + ")")),
            Me.Unit)
    End Function
End Class

Public MustInherit Class ConditionCombinator
    Inherits ExpressionsCombinator

    Private NotInheritable Class AndConditionCombinator
        Inherits ConditionCombinator

        Public Overrides ReadOnly Property Unit As String
            Get
                Return "0 = 0"
            End Get
        End Property

        Protected Overrides ReadOnly Property Combinator As String
            Get
                Return "and"
            End Get
        End Property
    End Class

    Private NotInheritable Class OrConditionCombinator
        Inherits ConditionCombinator

        Public Overrides ReadOnly Property Unit As String
            Get
                Return "0 = 1"
            End Get
        End Property

        Protected Overrides ReadOnly Property Combinator As String
            Get
                Return "or"
            End Get
        End Property
    End Class

    Private Sub New()
    End Sub

    Public Shared ReadOnly [And] As ConditionCombinator = New AndConditionCombinator()
    Public Shared ReadOnly [Or] As ConditionCombinator = New OrConditionCombinator()
End Class
