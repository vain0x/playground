Imports System.Runtime.CompilerServices

''' <summary>
''' 値の再設定をイベントとして通知するプロパティを表します。
''' TODO: IObservable(Of X) を実装する。
''' </summary>
Partial Public MustInherit Class ObservableProperty(Of X)
    Public Overridable ReadOnly Property CanRead As Boolean
        Get
            Return True
        End Get
    End Property

    Public Overridable ReadOnly Property CanWrite As Boolean
        Get
            Return True
        End Get
    End Property

    Public MustOverride Property Value As X

    Public Class ChangedEventArgs
        Public ReadOnly [Property] As ObservableProperty(Of X)
        Public ReadOnly OldValue As X
        Public ReadOnly NewValue As X

        Public Sub New([property] As ObservableProperty(Of X), oldValue As X, newValue As X)
            Me.Property = [property]
            Me.OldValue = oldValue
            Me.NewValue = newValue
        End Sub
    End Class

    Public Event Changed As EventHandler(Of ChangedEventArgs)

    Protected Sub ForceRaiseChanged()
        Dim value = Me.Value
        RaiseEvent Changed(Me, New ChangedEventArgs(Me, value, value))
    End Sub

    Protected Sub RaiseChanged(oldValue As X, newValue As X)
        If Equals(oldValue, newValue) Then Return
        RaiseEvent Changed(Me, New ChangedEventArgs(Me, oldValue, newValue))
    End Sub
End Class

''' <summary>
''' 代入操作によってのみ値が変化する観測可能プロパティを表します。
''' </summary>
Public Class VariableObservableProperty(Of X)
    Inherits ObservableProperty(Of X)

    Private _value As X

    Public Overrides Property Value As X
        Get
            Return Me._value
        End Get
        Set(value As X)
            Dim oldValue = Me._value
            Me._value = value
            RaiseChanged(oldValue, value)
        End Set
    End Property

    Public Sub New(value As X)
        Me._value = value
    End Sub

    Public Sub New()
        ' Me._value = default
    End Sub
End Class

''' <summary>
''' CLRプロパティと等価な観測可能プロパティを表します。
''' </summary>
Public Class RelayObservableProperty(Of X)
    Inherits ObservableProperty(Of X)

    Private ReadOnly _get As Func(Of X)
    Private ReadOnly _setOrNull As Action(Of X)

    Public Overrides ReadOnly Property CanWrite As Boolean
        Get
            Return _setOrNull IsNot Nothing
        End Get
    End Property

    Public Overrides Property Value As X
        Get
            Return Me._get()
        End Get
        Set(value As X)
            Dim oldValue = Me.Value
            Me._setOrNull(value)
            Me.RaiseChanged(oldValue, Me.Value)
        End Set
    End Property

    Public Sub New([get] As Func(Of X), setOrNull As Action(Of X))
        Debug.Assert([get] IsNot Nothing)
        Me._get = [get]
        Me._setOrNull = setOrNull
    End Sub

    Public Sub New([get] As Func(Of X))
        Me.New([get], Nothing)
    End Sub
End Class

Public Class ReadOnlyObservableProperty(Of X)
    Inherits ObservableProperty(Of X)

    Private ReadOnly _property As ObservableProperty(Of X)

    Public Overrides ReadOnly Property CanRead As Boolean
        Get
            Return Me._property.CanRead
        End Get
    End Property

    Public Overrides ReadOnly Property CanWrite As Boolean
        Get
            Return False
        End Get
    End Property

    Public Overrides Property Value As X
        Get
            Return Me._property.Value
        End Get
        Set(value As X)
            Throw New NotSupportedException()
        End Set
    End Property

    Public Sub New([property] As ObservableProperty(Of X))
        Me._property = [property]
    End Sub
End Class

Public Class HistoryObservableProperty(Of X)
    Inherits ObservableProperty(Of IList(Of X))

    Private ReadOnly _source As ObservableProperty(Of X)
    Private ReadOnly _hisotry As New List(Of X)()

    Public Overrides ReadOnly Property CanRead As Boolean
        Get
            Return Me._source.CanRead
        End Get
    End Property

    Public Overrides ReadOnly Property CanWrite As Boolean
        Get
            Return Me._source.CanWrite
        End Get
    End Property

    Public Overrides Property Value As IList(Of X)
        Get
            Return Me._hisotry
        End Get
        Set(value As IList(Of X))
            Throw New NotSupportedException()
        End Set
    End Property

    Private Sub OnSourceChanged(sender As Object, e As ObservableProperty(Of X).ChangedEventArgs)
        Me._hisotry.Add(e.NewValue)
        ForceRaiseChanged()
    End Sub

    Public Sub New([property] As ObservableProperty(Of X))
        Me._source = [property]
        Me._hisotry.Add(Me._source.Value)
        AddHandler Me._source.Changed, AddressOf Me.OnSourceChanged
    End Sub
End Class

''' <summary>
''' 値として関数の結果を返す観測可能プロパティを表します。
''' </summary>
Public Class MapObservableProperty(Of X, Y)
    Inherits ObservableProperty(Of Y)

    Private ReadOnly _source As ObservableProperty(Of X)
    Private ReadOnly _property As ObservableProperty(Of Y)
    Private ReadOnly _f As Func(Of X, Y)

    Public Overrides ReadOnly Property CanWrite As Boolean
        Get
            Return False
        End Get
    End Property

    Public Overrides Property Value As Y
        Get
            Return Me._property.Value
        End Get
        Set(value As Y)
            Throw New NotSupportedException()
        End Set
    End Property

    Private Sub OnSourceChanged(sender As Object, e As ObservableProperty(Of X).ChangedEventArgs)
        Me._property.Value = Me._f(e.NewValue)
    End Sub

    Private Sub OnValueChanged(sender As Object, e As ObservableProperty(Of Y).ChangedEventArgs)
        RaiseChanged(e.OldValue, e.NewValue)
    End Sub

    Public Sub New(source As ObservableProperty(Of X), f As Func(Of X, Y))
        Me._f = f
        Me._source = source
        Me._property = New VariableObservableProperty(Of Y)() With {.Value = Me._f(Me._source.Value)}

        AddHandler Me._source.Changed, AddressOf OnSourceChanged
        AddHandler Me._property.Changed, AddressOf OnValueChanged
    End Sub
End Class

Public Class BimapObservableProperty(Of X, Y)
    Inherits ObservableProperty(Of Y)

    Private ReadOnly _source As ObservableProperty(Of X)
    Private ReadOnly _convert As Func(Of X, Y)
    Private ReadOnly _invert As Func(Of Y, X)

    Public Overrides ReadOnly Property CanRead As Boolean
        Get
            Return Me._source.CanRead
        End Get
    End Property

    Public Overrides ReadOnly Property CanWrite As Boolean
        Get
            Return Me._source.CanWrite
        End Get
    End Property

    Public Overrides Property Value As Y
        Get
            Return Me._convert(Me._source.Value)
        End Get
        Set(value As Y)
            Dim oldValue = Me.Value
            Me._source.Value = Me._invert(value)
            Me.RaiseChanged(oldValue, value)
        End Set
    End Property

    Private Sub OnSourceChanged(sender As Object, e As ObservableProperty(Of X).ChangedEventArgs)
        Me.RaiseChanged(Me._convert(e.OldValue), Me._convert(e.NewValue))
    End Sub

    Public Sub New(source As ObservableProperty(Of X), convert As Func(Of X, Y), invert As Func(Of Y, X))
        Me._source = source
        Me._convert = convert
        Me._invert = invert

        AddHandler Me._source.Changed, AddressOf OnSourceChanged
    End Sub
End Class

Public Class FlattenObservableProperty(Of X)
    Inherits ObservableProperty(Of X)

    Private ReadOnly _property As ObservableProperty(Of ObservableProperty(Of X))

    Public Overrides ReadOnly Property CanRead As Boolean
        Get
            Return Me._property.Value.CanRead
        End Get
    End Property

    Public Overrides ReadOnly Property CanWrite As Boolean
        Get
            Return Me._property.Value.CanWrite
        End Get
    End Property

    Public Overrides Property Value As X
        Get
            Return Me._property.Value.Value
        End Get
        Set(value As X)
            Me._property.Value.Value = value
        End Set
    End Property

    Private Sub OnOuterValueChanged(sender As Object, e As ObservableProperty(Of ObservableProperty(Of X)).ChangedEventArgs)
        RaiseChanged(e.OldValue.Value, e.NewValue.Value)
        AddHandler Me._property.Value.Changed, AddressOf OnInnerValueChanged
    End Sub

    Private Sub OnInnerValueChanged(sender As Object, e As ObservableProperty(Of X).ChangedEventArgs)
        RaiseChanged(e.OldValue, e.NewValue)
    End Sub

    Public Sub New([property] As ObservableProperty(Of ObservableProperty(Of X)))
        Me._property = [property]

        AddHandler Me._property.Changed, AddressOf OnOuterValueChanged
        AddHandler Me._property.Value.Changed, AddressOf OnInnerValueChanged
    End Sub
End Class

Public Module ObservablePropertyExtensions
    <Extension>
    Public Function MakeReadOnly(Of X)(this As ObservableProperty(Of X)) As ObservableProperty(Of X)
        Return New ReadOnlyObservableProperty(Of X)(this)
    End Function

    <Extension>
    Public Function History(Of X)(this As ObservableProperty(Of X)) As ObservableProperty(Of IList(Of X))
        Return New HistoryObservableProperty(Of X)(this)
    End Function

    <Extension>
    Public Function Map(Of X, Y)(this As ObservableProperty(Of X), f As Func(Of X, Y)) As ObservableProperty(Of Y)
        Return New MapObservableProperty(Of X, Y)(this, f)
    End Function

    <Extension>
    Public Function Bimap(Of X, Y)(this As ObservableProperty(Of X), convert As Func(Of X, Y), invert As Func(Of Y, X)) As ObservableProperty(Of Y)
        Return New BimapObservableProperty(Of X, Y)(this, convert, invert)
    End Function

    <Extension>
    Public Function Flatten(Of X)(this As ObservableProperty(Of ObservableProperty(Of X))) As ObservableProperty(Of X)
        Return New FlattenObservableProperty(Of X)(this)
    End Function

    <Extension>
    Public Function Bind(Of X, Y)(this As ObservableProperty(Of X), f As Func(Of X, ObservableProperty(Of Y))) As ObservableProperty(Of Y)
        Return this.Map(f).Flatten()
    End Function
End Module

Partial Class ObservableProperty(Of X)
    Public Function [Select](Of Y)(f As Func(Of X, Y)) As ObservableProperty(Of Y)
        Return Me.Map(f)
    End Function

    Public Function SelectMany(Of Y)(f As Func(Of X, ObservableProperty(Of Y))) As ObservableProperty(Of Y)
        Return Me.Bind(f)
    End Function

    Public Function SelectMany(Of Y, Z)(f As Func(Of X, ObservableProperty(Of Y)), run As Func(Of X, Y, Z)) As ObservableProperty(Of Z)
        Return Me.Bind(Function(valueX) f(valueX).Map(Function(valueY) run(valueX, valueY)))
    End Function
End Class

Public Class ObservableProperty
    Public Shared Function Create(Of X)() As ObservableProperty(Of X)
        Return New VariableObservableProperty(Of X)()
    End Function

    Public Shared Function Create(Of X)(value As X) As ObservableProperty(Of X)
        Return New VariableObservableProperty(Of X)(value)
    End Function

    Public Shared Function CreateConst(Of X)(value As X) As ObservableProperty(Of X)
        Return Create(value).MakeReadOnly()
    End Function

    Public Shared Function CreateReadOnly(Of X)([get] As Func(Of X)) As ObservableProperty(Of X)
        Return New RelayObservableProperty(Of X)([get])
    End Function

    Public Shared Function Create(Of X)([get] As Func(Of X), [set] As Action(Of X)) As ObservableProperty(Of X)
        Debug.Assert([set] IsNot Nothing)
        Return New RelayObservableProperty(Of X)([get], [set])
    End Function
End Class
