Public MustInherit Class [Option](Of X)
    Implements IEnumerable(Of X)

    Public MustOverride ReadOnly Property HasValue As Boolean
    Public MustOverride ReadOnly Property Value As X

    Public Function TryGetValue(ByRef result As X) As Boolean
        If HasValue Then
            result = Value
            Return True
        Else
            Return False
        End If
    End Function

    Public Function ValueOr(alternative As X) As X
        Return If(HasValue, Value, alternative)
    End Function

    Public Function ValueOrElse(f As Func(Of X)) As X
        Return If(HasValue, Value, f())
    End Function

    Public Function Map(Of Y)(f As Func(Of X, Y)) As [Option](Of Y)
        Return If(HasValue, [Option].Some(f(Value)), [Option].None(Of Y)())
    End Function

    Public Function Bind(Of Y)(f As Func(Of X, [Option](Of Y))) As [Option](Of Y)
        Return If(HasValue, f(Value), [Option].None(Of Y)())
    End Function

    Private Iterator Function Enumerate() As IEnumerable(Of X)
        If HasValue Then Yield Value
    End Function

    Public Function GetEnumerator() As IEnumerator(Of X) Implements IEnumerable(Of X).GetEnumerator
        Return Enumerate().GetEnumerator()
    End Function

    Private Function NongenericGetEnumerator() As IEnumerator Implements IEnumerable.GetEnumerator
        Return Enumerate().GetEnumerator()
    End Function

    <DebuggerDisplay("Some ({Value})")>
    Public Class Some
        Inherits [Option](Of X)

        Public Overrides ReadOnly Property HasValue As Boolean
            Get
                Return True
            End Get
        End Property

        Private _value As X

        Public Overrides ReadOnly Property Value As X
            Get
                Return _value
            End Get
        End Property

        Public Sub New(value As X)
            _value = value
        End Sub
    End Class

    <DebuggerDisplay("None")>
    Public Class None
        Inherits [Option](Of X)

        Public Overrides ReadOnly Property HasValue As Boolean
            Get
                Return False
            End Get
        End Property

        Public Overrides ReadOnly Property Value As X
            Get
                Throw New NullReferenceException()
            End Get
        End Property

        Private Sub New()
        End Sub

        Public Shared ReadOnly Instance As None = New None()
    End Class
End Class

Namespace [Option]
    <HideModuleName>
    Public Module [Option]
        Public Function Some(Of X)(value As X) As [Option](Of X)
            Return New [Option](Of X).Some(value)
        End Function

        Public Function None(Of X)() As [Option](Of X)
            Return [Option](Of X).None.Instance
        End Function
    End Module
End Namespace
