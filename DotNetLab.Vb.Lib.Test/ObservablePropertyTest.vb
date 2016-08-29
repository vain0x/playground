Imports System.Runtime.CompilerServices
Imports Xunit
Imports DotNetLab.Vb.Lib

Public Class ObservablePropertyTest
    Public Function History(Of X)(this As IObservableProperty(Of X)) As List(Of X)
        Dim values = New List(Of X)()
        this.Subscribe(Sub(value) values.Add(value))
        Return values
    End Function

    <Fact>
    Public Sub CreateTest()
        Dim op = ObservableProperty.Create(0)
        Assert.Equal(0, op.Value)

        Dim history = Me.History(op)
        Assert.Equal({0}, history)

        op.Value = 1
        Assert.Equal(1, op.Value)

        op.Value = 2
        Assert.Equal({0, 1, 2}, history)
    End Sub

    <Fact>
    Public Sub MakeReadOnlyTest()
        Dim source = ObservableProperty.Create(0)
        Dim op = source.MakeReadOnly()
        Assert.Equal(0, op.Value)
        Assert.Equal(False, op.CanWrite)
        Assert.ThrowsAny(Of Exception)(Sub() op.Value = 1)
        source.Value = 1
        Assert.Equal(1, op.Value)
    End Sub

    <Fact>
    Public Sub SelectTest()
        Dim source = ObservableProperty.Create(0)
        Dim dependent = source.Select(Function(x) (x \ 2).ToString())
        Dim dependentHistory = Me.History(dependent)

        Assert.Equal("0", dependent.Value)
        source.Value = 1
        source.Value = 2
        source.Value = 3
        Assert.Equal("1", dependent.Value)
        Assert.Equal({"0", "0", "1", "1"}, dependentHistory)
    End Sub

    <Fact>
    Public Sub FlattenTest()
        Dim source = ObservableProperty.Create(ObservableProperty.Create(0))
        Dim dependent = source.Flatten()
        Assert.Equal(0, dependent.Value)
        ' 1. Whenever inner value changes, flattened proeprty changes.
        source.Value.Value = 1
        Assert.Equal(1, dependent.Value)
        ' 2. Whenever outer property changes, flattened property changes.
        source.Value = ObservableProperty.Create(2)
        Assert.Equal(2, dependent.Value)
        ' 3. And still has property 1.
        source.Value.Value = 3
        Assert.Equal(3, dependent.Value)
    End Sub

    <Fact>
    Public Sub AggregateTest()
        Dim sources =
            Enumerable.Range(0, 3) _
            .Select(Function(i) ObservableProperty.Create(i)) _
            .ToArray()
        Dim dependent = sources.Aggregate(
            Function() String.Empty,
            Function(y, x) y + "," + x.ToString())
        Dim dependentHistory = Me.History(dependent)

        sources(0).Value = 5
        sources(1).Value = 6
        Assert.Equal(
            {
                ",0,1,2",
                ",5,1,2",
                ",5,6,2"
            }, dependentHistory)
    End Sub

    <Fact>
    Public Sub MakeUndoableUndoTest()
        Dim undoable = ObservableProperty.Create(0).MakeUndoable()
        Assert.True(Not undoable.CanUndo)
        Assert.ThrowsAny(Of Exception)(Sub() undoable.Undo())
        undoable.Value = 1
        Assert.True(undoable.CanUndo)
        undoable.Undo() ' To 0.
        Assert.Equal(0, undoable.Value)
        Assert.True(Not undoable.CanUndo)
        undoable.Value = 2
        undoable.Value = 3
        undoable.Undo() ' To 2.
        undoable.Value = 4
        undoable.Undo() ' To 2 again.
        Assert.Equal(2, undoable.Value)
    End Sub

    <Fact>
    Public Sub MakeUndoableRedoTest()
        Dim undoable = ObservableProperty.Create(0).MakeUndoable()
        Assert.True(Not undoable.CanRedo)
        Assert.ThrowsAny(Of Exception)(Sub() undoable.Redo())
        undoable.Value = 1
        undoable.Value = 2
        Assert.True(Not undoable.CanRedo)
        undoable.Undo() ' To 1.
        Assert.True(undoable.CanRedo)
        undoable.Redo() ' To 2.
        Assert.Equal(2, undoable.Value)
        undoable.Undo() ' To 1.
        undoable.Value = 3
        Assert.True(Not undoable.CanRedo)
    End Sub

    <Fact>
    Public Sub MakeUndoableIsOriginalTest()
        Dim undoable = ObservableProperty.Create(0).MakeUndoable()
        Assert.True(undoable.IsOriginal)
        undoable.Value = 1
        Assert.True(Not undoable.IsOriginal)
        undoable.Value = 0
        Assert.True(undoable.IsOriginal)
        undoable.Undo()  ' To 1.
        undoable.MarkAsOriginal()
        Assert.True(undoable.IsOriginal)
        undoable.Undo()
        Assert.True(Not undoable.IsOriginal)
    End Sub
End Class
