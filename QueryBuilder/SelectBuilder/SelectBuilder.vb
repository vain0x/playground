Imports System.Data.Common
Imports QueryBuilder.Detail

Public Class SelectBuilder
    Inherits BuilderBase(Of SelectBuilder)

    Private ReadOnly _parameterFactory As IDbParameterFactory
    Private ReadOnly _tables As New List(Of OptionallyAliased)()
    Private ReadOnly _fields As New List(Of OptionallyAliased)()
    Private _joins As List(Of [Join]) = Nothing
    Private _orderKeys As List(Of Tuple(Of String, OrderDirection)) = Nothing
    Private _groupKeys As List(Of String) = Nothing
    Private _whereConditions As ConditionBuilder = Nothing
    Private _havingConditions As ConditionBuilder = Nothing
    Private _parameters As New List(Of DbParameter)()

    Public Sub New(parameterFactory As IDbParameterFactory)
        Me._parameterFactory = parameterFactory
    End Sub

    Public Function From(tableName As String, Optional aliasOrNull As String = Nothing) As SelectBuilder
        Me._tables.Add(New OptionallyAliased(tableName, aliasOrNull))
        Return Me
    End Function

    Private Function JoinOnImpl(
        joinType As String,
        tableName As String,
        tableNameAliasOrNull As String,
        condition As ConditionBuilder
        ) As SelectBuilder
        Debug.Assert(condition IsNot Nothing)
        If Me._joins Is Nothing Then Me._joins = New List(Of [Join])()
        Me._joins.Add(New [Join](joinType, New OptionallyAliased(tableName, tableNameAliasOrNull), condition.ToString()))
        Me._parameters.AddRange(condition.Parameters)
        Return Me
    End Function

    Public Function JoinAsOn(tableName As String, aliasOrNull As String, condition As ConditionBuilder) As SelectBuilder
        Return Me.JoinOnImpl("join", tableName, aliasOrNull, condition)
    End Function

    Public Function JoinOn(tableName As String, condition As ConditionBuilder) As SelectBuilder
        Return Me.JoinAsOn(tableName, Nothing, condition)
    End Function

    Public Function Field(expression As String, aliasOrNull As String) As SelectBuilder
        Me._fields.Add(New OptionallyAliased(expression, aliasOrNull))
        Return Me
    End Function

    Public Function Field(expression As String) As SelectBuilder
        Return Me.Field(expression, aliasOrNull:=Nothing)
    End Function

    Public Function FieldMany(expressions As IEnumerable(Of String)) As SelectBuilder
        Debug.Assert(expressions IsNot Nothing)
        Me._fields.AddRange(expressions.Select(Function(e) New OptionallyAliased(e)))
        Return Me
    End Function

    Public Function Where(condition As ConditionBuilder) As SelectBuilder
        Debug.Assert(condition IsNot Nothing)
        If Me._whereConditions Is Nothing Then
            Me._whereConditions = New ConditionBuilder(ConditionCombinator.And, Me._parameterFactory)
        End If
        Me._whereConditions.Add(condition)
        Return Me
    End Function

    Public Function GroupBy(expression As String) As SelectBuilder
        Debug.Assert(expression IsNot Nothing)
        If Me._groupKeys Is Nothing Then
            Me._groupKeys = New List(Of String)()
        End If
        Me._groupKeys.Add(expression)
        Return Me
    End Function

    Private Function AddOrderKey(expression As String, direction As OrderDirection) As SelectBuilder
        Debug.Assert(expression IsNot Nothing)
        If Me._orderKeys Is Nothing Then
            Me._orderKeys = New List(Of Tuple(Of String, OrderDirection))()
        End If
        Me._orderKeys.Add(Tuple.Create(expression, direction))
        Return Me
    End Function

    Public Function OrderBy(expression As String) As SelectBuilder
        Return Me.AddOrderKey(expression, OrderDirection.Ascending)
    End Function

    Public Function OrderByDesc(expression As String) As SelectBuilder
        Return Me.AddOrderKey(expression, OrderDirection.Descending)
    End Function

    Public Function Having(condition As ConditionBuilder) As SelectBuilder
        Debug.Assert(condition IsNot Nothing)
        If Me._havingConditions Is Nothing Then
            Me._havingConditions = New ConditionBuilder(ConditionCombinator.And, Me._parameterFactory)
        End If
        Me._havingConditions.Add(condition)
        Return Me
    End Function

    Private Class SelectQueryEmitter
        Inherits SqlEmitter

        Public Sub AppendFieldList(fields As IEnumerable(Of OptionallyAliased))
            Debug.Assert(fields IsNot Nothing)
            Dim fieldList = If(fields.Any(), fields, {New OptionallyAliased("*")})
            Me.AppendConcat("select", ",", fieldList, Sub(f) Me.AppendAlias(f))
        End Sub

        Private Sub AppendJoin([join] As [Join])
            Me.AppendLine([join].JoinType)
            Me.AppendAlias([join].TableName)
            Me.AppendLine("on")
            Me.AppendLine([join].OnExpression)
        End Sub

        Public Sub AppendFrom(tables As IEnumerable(Of OptionallyAliased), joins As IEnumerable(Of [Join]))
            If Not tables.Any() Then
                Throw New SyntaxException("SelectBulider.From must be invoked once at least.")
            End If

            Me.AppendConcat("from", ",", tables, Sub(table) Me.AppendAlias(table))

            If joins IsNot Nothing Then
                For Each join In joins
                    Me.AppendJoin(join)
                Next
            End If
        End Sub

        Public Sub AppendWhere(condition As ConditionBuilder)
            Me.AppendConditions("where", condition)
        End Sub

        Public Sub AppendHaving(condition As ConditionBuilder)
            Me.AppendConditions("having", condition)
        End Sub

        Public Sub AppendGroupBy(groupKeys As IEnumerable(Of String))
            Me.AppendConcat("group by", ",", groupKeys, Sub(e) Me.AppendLine(e))
        End Sub

        Private Sub AppendOrderKey(expression As String, direction As OrderDirection)
            Me.AppendLine(expression)
            Select Case direction
                Case OrderDirection.Ascending
                    ' Emit nothing.
                Case OrderDirection.Descending
                    Me.AppendLine("desc")
            End Select
        End Sub

        Public Sub AppendOrderBy(orderKeys As IEnumerable(Of Tuple(Of String, OrderDirection)))
            Me.AppendConcat(
                "order by", ",", orderKeys,
                Sub(t) Me.AppendOrderKey(t.Item1, t.Item2))
        End Sub
    End Class

    Public Overrides Function ToString() As String
        Dim sql = New SelectQueryEmitter()
        sql.AppendFieldList(Me._fields)
        sql.AppendFrom(Me._tables, Me._joins)
        sql.AppendWhere(Me._whereConditions)
        sql.AppendGroupBy(Me._groupKeys)
        sql.AppendHaving(Me._havingConditions)
        sql.AppendOrderBy(Me._orderKeys)
        Return sql.ToString()
    End Function

    Public ReadOnly Property Parameters As IEnumerable(Of DbParameter)
        Get
            Return Me._parameters
        End Get
    End Property
End Class
