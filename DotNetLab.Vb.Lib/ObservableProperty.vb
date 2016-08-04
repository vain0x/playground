Imports System.Collections.Concurrent
Imports System.Threading
Imports System.Runtime.CompilerServices

Public Interface IObservableProperty(Of TValue)
    Inherits IObservable(Of TValue)
    Inherits IDisposable

    ReadOnly Property CanRead As Boolean
    ReadOnly Property CanWrite As Boolean
    Property Value As TValue
End Interface

Namespace Detail
    Public Class Disposable
        Public Shared Sub Dispose(ByRef disposableRef As IDisposable)
            Dim disposable = Interlocked.Exchange(disposableRef, Nothing)
            If disposable IsNot Nothing Then disposable.Dispose()
        End Sub
    End Class

    Public Class AnonymousDisposable
        Implements IDisposable

        Private _dispose As Action

        Public Sub Dispose() Implements IDisposable.Dispose
            Dim dispose = Interlocked.Exchange(Me._dispose, Nothing)
            If dispose IsNot Nothing Then dispose()
        End Sub

        Public Sub New(dispose As Action)
            Me._dispose = dispose
        End Sub
    End Class

    Public Class AnonymousObserver(Of TValue)
        Implements IObserver(Of TValue)

        ' TODO: OnError, OnCompleted
        Private ReadOnly _onNext As Action(Of TValue)

#Region "IObserver"
        Public Sub OnNext(value As TValue) Implements IObserver(Of TValue).OnNext
            Me._onNext(value)
        End Sub

        Public Sub OnError([error] As Exception) Implements IObserver(Of TValue).OnError
        End Sub

        Public Sub OnCompleted() Implements IObserver(Of TValue).OnCompleted
        End Sub
#End Region

        Public Sub New(onNext As Action(Of TValue))
            Me._onNext = onNext
        End Sub
    End Class
End Namespace

Public Module ObservableExtensions
    <Extension>
    Public Function Subscribe(Of X)(observable As IObservable(Of X), onNext As Action(Of X)) As IDisposable
        Return observable.Subscribe(New Detail.AnonymousObserver(Of X)(onNext))
    End Function
End Module

Namespace Detail
    Public Class Observable(Of TValue)
        Implements IObservable(Of TValue)

#Region "IObservable"
        Private _nextSubscriberId As Integer = Integer.MinValue
        Private ReadOnly _subscribers As IDictionary(Of Integer, IObserver(Of TValue)) =
            New ConcurrentDictionary(Of Integer, IObserver(Of TValue))()

        Public Overridable Function Subscribe(observer As IObserver(Of TValue)) As IDisposable Implements IObservable(Of TValue).Subscribe
            Dim id = Interlocked.Increment(Me._nextSubscriberId)
            Me._subscribers.Add(id, observer)
            Return New AnonymousDisposable(Sub() Me._subscribers.Remove(id))
        End Function
#End Region

#Region "Notifying methods"
        ' TODO: OnError, OnCompleted

        Protected Sub NotifyNext(value As TValue)
            For Each observer In Me._subscribers.Values
                observer.OnNext(value)
            Next
        End Sub
#End Region
    End Class

    Public MustInherit Class ObservableProperty(Of TValue)
        Inherits Observable(Of TValue)
        Implements IObservableProperty(Of TValue)

#Region "IObservableProperty"
        Public Overridable ReadOnly Property CanRead As Boolean Implements IObservableProperty(Of TValue).CanRead
            Get
                Return True
            End Get
        End Property

        Public Overridable ReadOnly Property CanWrite As Boolean Implements IObservableProperty(Of TValue).CanWrite
            Get
                Return True
            End Get
        End Property

        Public MustOverride Property Value As TValue Implements IObservableProperty(Of TValue).Value
#End Region

#Region "IDisposable"
        Protected Overridable Sub Dispose() Implements IDisposable.Dispose
        End Sub
#End Region

        Public Overrides Function Subscribe(observer As IObserver(Of TValue)) As IDisposable
            observer.OnNext(Me.Value)
            Return MyBase.Subscribe(observer)
        End Function
    End Class

    Public Class VariableObservableProperty(Of TValue)
        Inherits ObservableProperty(Of TValue)

#Region "IObservableProperty"
        Private _value As TValue
        Public Overrides Property Value As TValue
            Get
                Return Me._value
            End Get
            Set(value As TValue)
                Me._value = value
                Me.NotifyNext(Me._value)
            End Set
        End Property
#End Region

        Public Sub New()
            ' Me._value = default
        End Sub

        Public Sub New(value As TValue)
            Me._value = value
        End Sub
    End Class

    Public Class ReadOnlyObservableProperty(Of TValue)
        Implements IObservableProperty(Of TValue)

#Region "IObservableProperty"
        Private ReadOnly _property As IObservableProperty(Of TValue)

        Public ReadOnly Property CanRead As Boolean Implements IObservableProperty(Of TValue).CanRead
            Get
                Return Me._property.CanRead
            End Get
        End Property

        Public ReadOnly Property CanWrite As Boolean Implements IObservableProperty(Of TValue).CanWrite
            Get
                Return False
            End Get
        End Property

        Public Property Value As TValue Implements IObservableProperty(Of TValue).Value
            Get
                Return Me._property.Value
            End Get
            Set(value As TValue)
                Throw New NotSupportedException()
            End Set
        End Property
#End Region

#Region "IObservable"
        Public Function Subscribe(observer As IObserver(Of TValue)) As IDisposable Implements IObservable(Of TValue).Subscribe
            Return Me._property.Subscribe(observer)
        End Function
#End Region

#Region "IDisposable"
        Private Sub Dispose() Implements IDisposable.Dispose
        End Sub
#End Region

        Public Sub New([property] As IObservableProperty(Of TValue))
            Debug.Assert([property] IsNot Nothing)
            Me._property = [property]
        End Sub
    End Class

    Public Class MapObservableProperty(Of TSource, TValue)
        Inherits ObservableProperty(Of TValue)
        Implements IObserver(Of TSource)
        Implements IDisposable

        Private ReadOnly _source As IObservableProperty(Of TSource)
        Private ReadOnly _f As Func(Of TSource, TValue)
        Private _subscribeToken As IDisposable

#Region "IObservableProperty"
        Public Overrides ReadOnly Property CanWrite As Boolean
            Get
                Return False
            End Get
        End Property

        Public Overrides Property Value As TValue
            Get
                Return Me._f(Me._source.Value)
            End Get
            Set(value As TValue)
                Throw New NotSupportedException()
            End Set
        End Property
#End Region

#Region "IObserver"
        Private Sub OnNext(value As TSource) Implements IObserver(Of TSource).OnNext
            MyBase.NotifyNext(Me.Value)
        End Sub

        Private Sub OnError([error] As Exception) Implements IObserver(Of TSource).OnError
            ' TODO: Support.
        End Sub

        Private Sub OnCompleted() Implements IObserver(Of TSource).OnCompleted
            ' TODO: Support.
        End Sub
#End Region

#Region "IDisposable"
        Protected Overrides Sub Dispose()
            Disposable.Dispose(Me._subscribeToken)
        End Sub
#End Region

        Public Sub New(source As IObservableProperty(Of TSource), f As Func(Of TSource, TValue))
            Debug.Assert(source IsNot Nothing)
            Debug.Assert(source.CanRead)
            Debug.Assert(f IsNot Nothing)
            Me._source = source
            Me._f = f
            Me._subscribeToken = Me._source.Subscribe(Me)
        End Sub
    End Class

    Public Class FlattenObservableProperty(Of TValue)
        Inherits ObservableProperty(Of TValue)

        Private ReadOnly _property As IObservableProperty(Of IObservableProperty(Of TValue))
        Private _outerSubscribeToken As IDisposable
        Private _innerSubscribeToken As IDisposable

#Region "IObservableProperty"
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

        Public Overrides Property Value As TValue
            Get
                Return Me._property.Value.Value
            End Get
            Set(value As TValue)
                Me._property.Value.Value = value
            End Set
        End Property
#End Region

#Region "IDisposable"
        Protected Overrides Sub Dispose()
            Disposable.Dispose(Me._outerSubscribeToken)
            Disposable.Dispose(Me._innerSubscribeToken)
        End Sub
#End Region

        Private Sub SubscribeInnerProperty([property] As IObservableProperty(Of TValue))
            Disposable.Dispose(Me._innerSubscribeToken)
            Me._innerSubscribeToken =
                [property].Subscribe(
                    Sub(value) Me.NotifyNext(value))
        End Sub

        Public Sub New([property] As IObservableProperty(Of IObservableProperty(Of TValue)))
            Me._property = [property]
            Me._outerSubscribeToken =
                Me._property.Subscribe(
                    AddressOf Me.SubscribeInnerProperty)
        End Sub
    End Class

    Public Class AggregateObservableProperty(Of TValue, TResult)
        Inherits ObservableProperty(Of TResult)
        Implements IObserver(Of TValue)

        Private ReadOnly _sources As IObservableProperty(Of TValue)()
        Private ReadOnly _subscribeTokens As IDisposable()
        Private ReadOnly _seed As Func(Of TResult)
        Private ReadOnly _aggregate As Func(Of TResult, TValue, TResult)

#Region "IObservableProperty"
        Public Overrides ReadOnly Property CanWrite As Boolean
            Get
                Return False
            End Get
        End Property

        Public Overrides Property Value As TResult
            Get
                Return Me._sources _
                    .Select(Function(source) source.Value) _
                    .Aggregate(Me._seed(), Me._aggregate)
            End Get
            Set(value As TResult)
                Throw New NotSupportedException()
            End Set
        End Property
#End Region

#Region "IDisposable"
        Public Sub OnNext(value As TValue) Implements IObserver(Of TValue).OnNext
            Me.NotifyNext(Me.Value)
        End Sub

        Public Sub OnCompleted() Implements IObserver(Of TValue).OnCompleted
            ' TODO: Support.
        End Sub

        Public Sub OnError([error] As Exception) Implements IObserver(Of TValue).OnError
            ' TODO: Support.
        End Sub
#End Region

#Region "IDisposable"
        Protected Overrides Sub Dispose()
            For Each subscribeToken In Me._subscribeTokens
                Disposable.Dispose(subscribeToken)
            Next
        End Sub
#End Region

        Public Sub New(sources As IEnumerable(Of IObservableProperty(Of TValue)), seed As Func(Of TResult), aggregate As Func(Of TResult, TValue, TResult))
            Debug.Assert(sources IsNot Nothing)
            Debug.Assert(seed IsNot Nothing)
            Debug.Assert(aggregate IsNot Nothing)
            Me._seed = seed
            Me._aggregate = aggregate
            Me._sources = sources.ToArray()

            Me._subscribeTokens =
                Me._sources _
                .Select(Function(source) _
                    source.Subscribe(Sub(value) Me.NotifyNext(Me.Value))) _
                .ToArray()

            Debug.Assert(Me._sources.All(Function(source) source.CanRead))
        End Sub
    End Class
End Namespace

Public Module ObservablePropertyExtensions
    <Extension>
    Public Function MakeReadOnly(Of X)(this As IObservableProperty(Of X)) As IObservableProperty(Of X)
        Return New Detail.ReadOnlyObservableProperty(Of X)(this)
    End Function

    <Extension>
    Public Function [Select](Of X, Y)(this As IObservableProperty(Of X), f As Func(Of X, Y)) As IObservableProperty(Of Y)
        Return New Detail.MapObservableProperty(Of X, Y)(this, f)
    End Function

    <Extension>
    Public Function Flatten(Of X)(this As IObservableProperty(Of IObservableProperty(Of X))) As IObservableProperty(Of X)
        Return New Detail.FlattenObservableProperty(Of X)(this)
    End Function

    <Extension>
    Public Function SelectMany(Of X, Y)(this As IObservableProperty(Of X), f As Func(Of X, IObservableProperty(Of Y))) As IObservableProperty(Of Y)
        Return this.Select(f).Flatten()
    End Function

    <Extension>
    Public Function SelectMany(Of X, Y, Z)(this As IObservableProperty(Of X), f As Func(Of X, IObservableProperty(Of Y)), run As Func(Of X, Y, Z)) As IObservableProperty(Of Z)
        Return _
            this.SelectMany(Function(valueX) _
                f(valueX).Select(Function(valueY) _
                    run(valueX, valueY)))
    End Function

    <Extension>
    Public Function Aggregate(Of X, Y)(this As IObservableProperty(Of X)(), seed As Func(Of Y), fold As Func(Of Y, X, Y)) As IObservableProperty(Of Y)
        Return New Detail.AggregateObservableProperty(Of X, Y)(this, seed, fold)
    End Function
End Module

Public Class ObservableProperty
    Public Shared Function Create(Of X)() As IObservableProperty(Of X)
        Return New Detail.VariableObservableProperty(Of X)()
    End Function

    Public Shared Function Create(Of X)(value As X) As IObservableProperty(Of X)
        Return New Detail.VariableObservableProperty(Of X)(value)
    End Function

    Public Shared Function CreateConst(Of X)(value As X) As IObservableProperty(Of X)
        Return Create(value).MakeReadOnly()
    End Function
End Class
