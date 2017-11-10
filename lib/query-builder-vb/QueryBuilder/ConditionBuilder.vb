Imports System.Data.Common

Public Class ConditionBuilder
    Inherits BuilderBase(Of ConditionBuilder)

    Private ReadOnly _combinator As ConditionCombinator
    Private ReadOnly _parameterFactory As IDbParameterFactory

    Public Sub New(combinator As ConditionCombinator, parameterFactory As IDbParameterFactory)
        Me._combinator = combinator
        Me._parameterFactory = parameterFactory
    End Sub

    Private Function FreshName() As String
        Return "__" + Guid.NewGuid().ToString().Replace("-", "")
    End Function

    Private ReadOnly _expressions As New List(Of String)()
    Private ReadOnly _parameters As New List(Of DbParameter)()

    Private Sub Add(expression As String)
        Me._expressions.Add(expression)
    End Sub

    Private Sub AddWithParameter(expression As String, name As String, value As Object)
        Me.Add(expression)
        Me._parameters.Add(Me._parameterFactory.Create(name, value))
    End Sub

    Public Function Add(condition As ConditionBuilder) As ConditionBuilder
        Me.Add(condition.ToString())
        Me._parameters.AddRange(condition.Parameters)
        Return Me
    End Function

    Public Function Equal(expression As String, value As Object) As ConditionBuilder
        Debug.Assert(expression IsNot Nothing)
        If value Is Nothing Then Throw New ArgumentNullException("value")

        Dim freshName = Me.FreshName()
        Me.AddWithParameter(
            String.Format("{0} = @{1}", expression, freshName),
            expression, value)
        Return Me
    End Function

    Public Function IsNull(expression As String) As ConditionBuilder
        Debug.Assert(expression IsNot Nothing)
        Dim freshName = Me.FreshName
        Me.Add(String.Format("{0} IS NULL", expression))
        Return Me
    End Function

    Public Overrides Function ToString() As String
        Return Me._combinator.Concat(Me._expressions)
    End Function

    Public ReadOnly Property Parameters As IEnumerable(Of DbParameter)
        Get
            Return Me._parameters
        End Get
    End Property

    Public ReadOnly Property IsTrivial As Boolean
        Get
            Return Not Me._expressions.Any()
        End Get
    End Property
End Class
