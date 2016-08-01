Imports DotNetLab.Vb.Lib.SheetObjectModel
Imports Xunit

Public Class PropertyTest
    <Fact>
    Public Sub VariablePropertyTest()
        Dim var = [Property].MakeVariable(1)
        Assert.True(var.CanRead)
        Assert.Equal(1, var.Value)
        Assert.True(var.CanWrite)
        var.Value = 2
        Assert.Equal(2, var.Value)
    End Sub

    <Fact>
    Public Sub VariablePropertySyncTest()
        Dim x = [Property].MakeVariable(1)
        Dim y = x
        x.Value = 2
        Assert.Equal(x.Value, y.Value)
    End Sub

    <Fact>
    Public Sub ToReadOnlyTest()
        Dim x = [Property].MakeVariable(1).ToReadOnly()
        Assert.False(x.CanWrite)
        Assert.ThrowsAny(Of Exception)(Sub() x.Value = 2)
    End Sub

    Public Class VariableObservablePropertyTester
        Public ReadOnly Vop As ObservableProperty(Of Integer)
        Public ReadOnly OldValues As New List(Of Integer)()

        Private Sub AddOldValue(sender As Object, e As ObservableProperty(Of Integer).ChangedEventArgs)
            Me.OldValues.Add(e.OldValue)
        End Sub

        Public Sub New()
            Me.Vop = [Property].MakeObservable(Of Integer)()
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
End Class
