Imports DotNetLab.Vb.Lib.SheetObjectModel
Imports Xunit

Public Class ObservablePropertyTest
    <Fact>
    Public Sub VariableObservablePropertyTest()
        Dim vop = ObservableProperty.Create(Of Integer)()
        Dim history = vop.History()

        Assert.Equal(0, vop.Value)
        For i = 1 To 3
            vop.Value = i
        Next
        Assert.Equal({0, 1, 2, 3}, history.Value.ToArray())
        Assert.Equal(3, vop.Value)
    End Sub

    <Fact>
    Public Sub RelayObservablePropertyTest()
        Dim theValue = 0

        Dim readWrite = ObservableProperty.Create(Of Integer)(
            Function() theValue,
            Sub(value) theValue = value)
        Dim history = readWrite.History()

        Dim [readOnly] = ObservableProperty.CreateReadOnly(Function() theValue)

        For i = 1 To 2
            Assert.Equal([readOnly].Value, [readWrite].Value)
            readWrite.Value = i
        Next
        Assert.Equal({0, 1, 2}, history.Value.ToArray())
    End Sub

    <Fact>
    Public Sub ObservablePropertyMakeReadOnlyTest()
        Dim x = ObservableProperty.Create(Of Integer)(0).MakeReadOnly()
        Assert.Equal(0, x.Value)
        Assert.ThrowsAny(Of Exception)(Sub() x.Value = 0)
    End Sub

    <Fact>
    Public Sub ObservablePropertyMapTest()
        Dim source = ObservableProperty.Create(0)
        Dim dependent = source.Map(Function(valueX) (valueX \ 2).ToString())
        Dim dependentHistory = dependent.History()
        For i = 2 To 4
            source.Value = i
        Next
        Assert.Equal({"0", "1", "1", "2"}, dependentHistory.Value.ToArray())
        Assert.Equal("2", dependent.Value)
        Assert.ThrowsAny(Of Exception)(Sub() dependent.Value = "1")
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

        Protected Sub AddHandlers()
            Me.Prefix.Subscribe(Sub(sender, value) Me.History.Add(Tuple.Create("Prefix", value)))
            Me.Price.Subscribe(Sub(sender, value) Me.History.Add(Tuple.Create("Price", value.ToString())))
            Me.Display.Subscribe(Sub(sender, value) Me.History.Add(Tuple.Create("Display", value)))
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
                    Tuple.Create("Prefix", "$"),
                    Tuple.Create("Price", "0"),
                    Tuple.Create("Display", "$0"),
                    Tuple.Create("Display", "$1.99"),
                    Tuple.Create("Display", "$1.99"),
                    Tuple.Create("Price", "1.99"),
                    Tuple.Create("Display", "$200"),
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

    <Fact>
    Public Sub ObservableAggregateTest()
        Dim sources = Enumerable.Range(0, 3).Select(Function(i) ObservableProperty.Create(i)).ToArray()
        Dim dependent = sources.Aggregate(Function() String.Empty, Function(y, x) y + "," + x.ToString())
        Dim dependentHistory = dependent.History()

        sources(0).Value = 5
        sources(1).Value = 6
        Assert.Equal(
            {
                ",0,1,2",
                ",5,1,2",
                ",5,6,2"
            }, dependentHistory.Value.ToArray())
    End Sub
End Class
