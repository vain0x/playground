Imports System.Drawing

Namespace SheetObjectModel
    ''' <summary>
    ''' スプレッドシートのビューモデルを表します。
    ''' </summary>
    Public MustInherit Class SheetViewModelBase
        Inherits SheetElementViewModel

        Private ReadOnly _cellTypeFactory As CellTypeFactory

        Public Overrides ReadOnly Property CellTypeFactory As CellTypeFactory
            Get
                Return Me._cellTypeFactory
            End Get
        End Property

        Public Sub New(cellTypeFactoryOrNull As CellTypeFactory)
            MyBase.New(parent:=Nothing, location:=New Point(0, 0))
            Me._cellTypeFactory = If(CellTypeFactory, CellTypeFactory.Default.Instance)
        End Sub

        Public Sub New()
            Me.New(Nothing)
        End Sub

        Public NotOverridable Overrides ReadOnly Property Root As SheetElementViewModel
            Get
                Return Me
            End Get
        End Property

        Public NotOverridable Overrides Function ContainsCellAt(location As Point) As Boolean
            Return True
        End Function

        Public Sub RaiseCellButtonClicked(cell As ICell)
            Me.RouteCellEvent(New RoutedCellEvent.ButtonClicked(cell))
        End Sub

        Public Sub RaiseCellValueChanged(cell As ICell, oldValue As Object, newValue As Object)
            Me.RouteCellEvent(New RoutedCellEvent.ValueChanged(cell, oldValue, newValue))
        End Sub
    End Class
End Namespace
