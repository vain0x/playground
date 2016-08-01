Imports System.Runtime.CompilerServices

Namespace SheetObjectModel
    ''' <summary>
    ''' プロパティを表します。
    ''' </summary>
    Public MustInherit Class [Property](Of X)
        Public MustOverride Property Value As X

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

        Public Overrides Function ToString() As String
            Return Me.Value.ToString()
        End Function
    End Class

    ''' <summary>
    ''' 変数と等価なプロパティを表します。
    ''' すなわち、値の取得と再設定は単に変数の値を取得、再設定するだけです。
    ''' </summary>
    Public Class VariableProperty(Of X)
        Inherits [Property](Of X)

        Public Overrides Property Value As X
    End Class

    Public Class ReadOnlyProperty(Of X)
        Inherits [Property](Of X)

        Private _property As [Property](Of X)

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

        Public Sub New([property] As [Property](Of X))
            Me._property = [property]
        End Sub

        Public Sub New(value As X)
            Me.New(New VariableProperty(Of X)() With {.Value = value})
        End Sub
    End Class

    Public Module PropertyExtensions
        <Extension>
        Public Function ToReadOnly(Of X)(this As [Property](Of X)) As [Property](Of X)
            Return New ReadOnlyProperty(Of X)(this)
        End Function
    End Module

    ''' <summary>
    ''' CLIプロパティと等価なプロパティを表します。
    ''' すなわち、値の取得と再設定がいずれも関数実行により実装されます。
    ''' </summary>
    ''' <typeparam name="X"></typeparam>
    ''' <remarks></remarks>
    Public Class RelayProperty(Of X)
        Inherits [Property](Of X)

        Private ReadOnly _get As Func(Of X)
        Private ReadOnly _set As Action(Of X)

        Public Overrides ReadOnly Property CanRead As Boolean
            Get
                Return Me._get IsNot Nothing
            End Get
        End Property

        Public Overrides ReadOnly Property CanWrite As Boolean
            Get
                Return Me._set IsNot Nothing
            End Get
        End Property

        Public Overrides Property Value As X
            Get
                Return Me._get()
            End Get
            Set(value As X)
                Me._set(value)
            End Set
        End Property

        Public Sub New([get] As Func(Of X), [set] As Action(Of X))
            Me._get = [get]
            Me._set = [set]
        End Sub
    End Class

    ''' <summary>
    ''' 値の再設定をイベントとして通知するプロパティを表します。
    ''' TODO: IObservable(Of X) を実装する。
    ''' </summary>
    Public MustInherit Class ObservableProperty(Of X)
        Inherits [Property](Of X)

        Public Class ChangedEventArgs
            Public ReadOnly [Property] As ObservableProperty(Of X)
            Public ReadOnly OldValue As [Option](Of X)
            Public ReadOnly NewValue As X

            Public Sub New([property] As ObservableProperty(Of X), oldValue As [Option](Of X), newValue As X)
                Me.Property = [property]
                Me.OldValue = oldValue
                Me.NewValue = newValue
            End Sub
        End Class

        Public Event Changed As EventHandler(Of ChangedEventArgs)

        Protected Sub RaiseChanged(sender As Object, e As ChangedEventArgs)
            RaiseEvent Changed(sender, e)
        End Sub

        Private _latestValueOrNone As [Option](Of X) = [Option].None(Of X)()

        Protected MustOverride Property ValueImpl As X

        Public Overrides Property Value As X
            Get
                Return Me.ValueImpl
            End Get
            Set(value As X)
                ' 前回と同じ値が設定される場合は、キャンセルします。
                If Me._latestValueOrNone.HasValue _
                    AndAlso Object.Equals(Me._latestValueOrNone.Value, value) _
                    Then Return

                ' 値を設定します。
                Dim oldValue = Me._latestValueOrNone
                Me._latestValueOrNone = [Option].Some(value)
                Me.ValueImpl = value

                ' 値の変更を通知します。
                RaiseEvent Changed(Me, New ChangedEventArgs(Me, oldValue, value))
            End Set
        End Property
    End Class

    ''' <summary>
    ''' 代入操作によってのみ値が変化する観測可能プロパティを表します。
    ''' </summary>
    ''' <typeparam name="X"></typeparam>
    Public Class VariableObservableProperty(Of X)
        Inherits ObservableProperty(Of X)

        Protected Overrides Property ValueImpl As X
    End Class

    ''' <summary>
    ''' 値として関数の結果を返す観測可能プロパティを表します。
    ''' </summary>
    ''' <typeparam name="X"></typeparam>
    ''' <typeparam name="Y"></typeparam>
    Public Class MapObservableProperty(Of X, Y)
        Inherits ObservableProperty(Of Y)

        Private ReadOnly _source As ObservableProperty(Of X)
        Private ReadOnly _f As Func(Of X, Y)

        Public Overrides ReadOnly Property CanWrite As Boolean
            Get
                Return False
            End Get
        End Property

        Protected Overrides Property ValueImpl As Y
            Get
                Return Me._f(_source.Value)
            End Get
            Set(value As Y)
                Throw New NotSupportedException()
            End Set
        End Property

        Private Sub OnSourceChanged(sender As Object, e As ObservableProperty(Of X).ChangedEventArgs)
            Dim eventArgs = New ChangedEventArgs(
                Me,
                e.OldValue.Map(Function(x) Me._f(x)),
                Me._f(e.NewValue))

            ' If oldValue = Some newValue then don't raise 'Changed' event.
            Dim isNotModified =
                eventArgs.OldValue _
                .Map(Function(oldValue) Equals(oldValue, eventArgs.NewValue)) _
                .SequenceEqual({True})
            If isNotModified Then Return

            RaiseChanged(Me, eventArgs)
        End Sub

        Public Sub New(source As ObservableProperty(Of X), f As Func(Of X, Y))
            Me._source = source
            Me._f = f

            AddHandler source.Changed, AddressOf OnSourceChanged
        End Sub
    End Class

    Public Module ObservablePropertyExtensions
        <Extension>
        Public Function Map(Of X, Y)(this As ObservableProperty(Of X), f As Func(Of X, Y)) As ObservableProperty(Of Y)
            Return New MapObservableProperty(Of X, Y)(this, f)
        End Function
    End Module

    Public Class [Property]
        Public Shared Function MakeVariable(Of X)(value As X) As VariableProperty(Of X)
            Return New VariableProperty(Of X)() With {.Value = value}
        End Function

        Public Shared Function MakeReadOnly(Of X)(value As X) As [Property](Of X)
            Return MakeVariable(value).ToReadOnly()
        End Function

        Public Shared Function MakeObservable(Of X)(value As X) As ObservableProperty(Of X)
            Return New VariableObservableProperty(Of X)() With {.Value = value}
        End Function
    End Class
End Namespace
