Imports DotNetLab.Vb.Lib.SheetObjectModel
Imports Xunit

Public Class PropertyTest
    Public Class VariableObservablePropertyTester
        Public ReadOnly Vop As ObservableProperty(Of Integer)
        Public ReadOnly OldValues As New List(Of Integer)()

        Private Sub AddOldValue(sender As Object, e As ObservableProperty(Of Integer).ChangedEventArgs)
            Me.OldValues.Add(e.OldValue)
        End Sub

        Public Sub New()
            Me.Vop = ObservableProperty.Create(Of Integer)()
            AddHandler Me.Vop.Changed, AddressOf Me.AddOldValue
        End Sub
    End Class

    <Fact>
    Public Sub VariableObservablePropertyTest()
        Dim tester = New VariableObservablePropertyTester()
        Assert.Equal(GetType(VariableObservableProperty(Of Integer)), tester.Vop.GetType())
        Assert.Equal(0, tester.Vop.Value)
        For i = 1 To 3
            tester.Vop.Value = i
        Next
        Assert.Equal({0, 1, 2}, tester.OldValues.ToArray())
        Assert.Equal(3, tester.Vop.Value)
    End Sub

    Public Class RelayObservablePropertyTester
        Public ReadOnly ReadWrite As ObservableProperty(Of Integer)
        Public ReadOnly [ReadOnly] As ObservableProperty(Of Integer)
        Public ReadOnly OldValues As New List(Of Integer)()

        Private Sub AddOldValue(sender As Object, e As ObservableProperty(Of Integer).ChangedEventArgs)
            Me.OldValues.Add(e.OldValue)
        End Sub

        Private _value As Integer

        Public Sub New()
            Me.ReadWrite = ObservableProperty.Create(Of Integer)(
                Function() Me._value,
                Sub(value) Me._value = value)
            Me.[ReadOnly] = ObservableProperty.CreateReadOnly(Of Integer)(Function() Me._value)

            AddHandler Me.[ReadOnly].Changed, AddressOf AddOldValue
            AddHandler Me.ReadWrite.Changed, AddressOf AddOldValue
        End Sub
    End Class

    <Fact>
    Public Sub RelayObservablePropertyTest()
        Dim tester = New RelayObservablePropertyTester()
        For i = 1 To 2
            Assert.Equal(tester.ReadOnly.Value, tester.ReadWrite.Value)
            tester.ReadWrite.Value = i
        Next
        Assert.Equal({0, 1}, tester.OldValues.ToArray())
    End Sub

    <Fact>
    Public Sub ObservablePropertyMakeReadOnlyTest()
        Dim x = ObservableProperty.Create(Of Integer)(0).MakeReadOnly()
        Assert.Equal(0, x.Value)
        Assert.ThrowsAny(Of Exception)(Sub() x.Value = 0)
    End Sub

    <Fact>
    Public Sub ObservablePropertyHistoryTest()
        Dim x = ObservableProperty.Create(0)
        Dim history = x.History()
        x.Value = 1
        x.Value = 2
        Assert.Equal({0, 1, 2}, history.Value.ToArray())
    End Sub

    Public Class ObservablePropertyMapTester
        Public ReadOnly Source As New VariableObservableProperty(Of Integer)(0)
        Public ReadOnly Dependent As ObservableProperty(Of String)
        Public ReadOnly Values As New List(Of String)()

        Private Sub OnValueChanged(sender As Object, e As ObservableProperty(Of String).ChangedEventArgs)
            Me.Values.Add(e.NewValue)
        End Sub

        Public Sub New()
            Me.Dependent = Me.Source.Map(Function(x) (x \ 2).ToString())
            AddHandler Me.Dependent.Changed, AddressOf OnValueChanged
        End Sub
    End Class

    <Fact>
    Public Sub ObservablePropertyMapTest()
        Dim tester = New ObservablePropertyMapTester()
        For i = 0 To 4
            tester.Source.Value = i
        Next
        Assert.Equal({"1", "2"}, tester.Values.ToArray())
        Assert.Equal("2", tester.Dependent.Value)
    End Sub

    <Fact>
    Public Sub ObservablePropertyBimapTest()
        Dim source = ObservableProperty.Create(0)
        Dim sourceHistory = source.History()
        Dim dependent = source.Bimap(Function(x) x.ToString(), Function(y) Int32.Parse(y))
        Dim dependentHistory = dependent.History()
        Assert.Equal("0", dependent.Value)
        ' dependent が変わるごとに、source も変わります。
        dependent.Value = "1"
        Assert.Equal("1", dependentHistory.Value.Last())
        Assert.Equal(1, sourceHistory.Value.Last())
        ' source が変わるごとに、dependent も変わります。
        source.Value = 2
        Assert.Equal(2, sourceHistory.Value.Last())
        Assert.Equal("2", dependentHistory.Value.Last())
    End Sub

    Public Class PriceDisplay
        Public Prefix As ObservableProperty(Of String)
        Public Price As ObservableProperty(Of Double)
        Public Display As ObservableProperty(Of String)

        Public ReadOnly History As New List(Of Tuple(Of String, String))()

        Public Sub OnPrefixChanged(sender As Object, e As ObservableProperty(Of String).ChangedEventArgs)
            Me.History.Add(Tuple.Create("Prefix", e.NewValue))
        End Sub

        Public Sub OnPriceChanged(sender As Object, e As ObservableProperty(Of Double).ChangedEventArgs)
            Me.History.Add(Tuple.Create("Price", e.NewValue.ToString()))
        End Sub

        Public Sub OnDisplayChanged(sender As Object, e As ObservableProperty(Of String).ChangedEventArgs)
            Me.History.Add(Tuple.Create("Display", e.NewValue))
        End Sub

        Protected Sub AddHandlers()
            AddHandler Me.Prefix.Changed, AddressOf OnPrefixChanged
            AddHandler Me.Price.Changed, AddressOf OnPriceChanged
            AddHandler Me.Display.Changed, AddressOf OnDisplayChanged
        End Sub

        Public Sub New()
            Me.Prefix = ObservableProperty.Create("$")
            Me.Price = ObservableProperty.Create(0.0)
        End Sub
    End Class

    Public Class ObservablePropertyBindTester
        Inherits PriceDisplay

        Private Sub Test()
            AddHandlers()

            Me.Price.Value = 1.99
            Me.Price.Value = 200
            Me.Prefix.Value = "Y."

            Assert.Equal(
                {
                    Tuple.Create("Display", "$1.99"),
                    Tuple.Create("Price", "1.99"),
                    Tuple.Create("Display", "$200"),
                    Tuple.Create("Price", "200"),
                    Tuple.Create("Display", "Y.200"),
                    Tuple.Create("Prefix", "Y.")
                }, Me.History.ToArray())
        End Sub

        Public Sub BindTest()
            Me.Display =
                Me.Prefix.Bind(Function(prefix) _
                Me.Price.Map(Function(value) _
                    String.Format("{0}{1}", prefix, value)))
            Test()
        End Sub

        Public Sub SelectAndSelectManyTest()
            Me.Display =
                From prefix In Me.Prefix
                From price In Me.Price
                Select String.Format("{0}{1}", prefix, price)
            Test()
        End Sub
    End Class

    <Fact>
    Public Sub ObservablePropertyBindTest()
        Dim tester = New ObservablePropertyBindTester()
        tester.BindTest()
    End Sub

    <Fact>
    Public Sub ObservablePropertyLinqTest()
        Dim tester = New ObservablePropertyBindTester()
        tester.SelectAndSelectManyTest()
    End Sub
End Class
