Namespace SheetObjectModel
    Public MustInherit Class SheetElementViewModel
        Private ReadOnly _parent As SheetElementViewModel

        ''' <summary>
        ''' このビューモデルの直接の親であるビューモデルを取得します。
        ''' (シート全体のビューモデルに対してのみ、null を返します。)
        ''' </summary>
        Public ReadOnly Property Parent As SheetElementViewModel
            Get
                Return Me._parent
            End Get
        End Property

        Public MustOverride ReadOnly Property Children As IEnumerable(Of SheetElementViewModel)

        Public Overridable ReadOnly Property Root As SheetElementViewModel
            Get
                Return Me.Parent.Root
            End Get
        End Property

        Private ReadOnly _location As Point

        ''' <summary>
        ''' このビューモデルが対応するセル群の左上隅の座標を取得します。
        ''' (スプレッドシート全体の絶対座標)
        ''' </summary>
        Public ReadOnly Property Location As Point
            Get
                Return Me._location
            End Get
        End Property

        Protected Overridable Function CalculateSize() As Size
            Dim nonemptyChildren =
                Me.Children.Where(Function(e) e.Size.Height > 0 And e.Size.Width > 0) _
                .ToArray()
            If Not nonemptyChildren.Any() Then Return New Size(0, 0)

            Dim maxWidth = nonemptyChildren.Select(Function(e) e.Location.X + e.Size.Width - Me.Location.X).Max()
            Dim maxHeight = nonemptyChildren.Select(Function(e) e.Location.Y + e.Size.Height - Me.Location.Y).Max()
            Return New Size(maxWidth, maxHeight)
        End Function

        Private ReadOnly _size As New Lazy(Of Size)(AddressOf CalculateSize)

        ''' <summary>
        ''' このビューモデルが対応するセル群の大きさを取得します。
        ''' 単位はセルの個数です。結果はキャッシュされます。
        ''' </summary>
        Public Overridable ReadOnly Property Size As Size
            Get
                Return Me._size.Value
            End Get
        End Property

        ''' <summary>
        ''' 絶対座標 location のセルがこのビューモデルの管轄であるかを取得します。
        ''' </summary>
        Public Overridable Function ContainsCellAt(location As Point) As Boolean
            Return Children.Any(Function(e) e.ContainsCellAt(location))
        End Function

        Public MustInherit Class RoutedCellEvent
            Public ReadOnly Cell As ICell

            Public Sub New(cell As ICell)
                Me.Cell = cell
            End Sub

            Public MustOverride Sub Handle(handler As Handler)

            Public Class ButtonClicked
                Inherits RoutedCellEvent

                Public Sub New(cell As ICell)
                    MyBase.New(cell)
                End Sub

                Public Overrides Sub Handle(handler As Handler)
                    handler.OnButtonClicked(Me)
                End Sub
            End Class

            Public Class ValueChanged
                Inherits RoutedCellEvent

                Public ReadOnly OldValue As Object
                Public ReadOnly NewValue As Object

                Public Sub New(cell As ICell, oldValue As Object, newValue As Object)
                    MyBase.New(cell)
                    Me.OldValue = oldValue
                    Me.NewValue = newValue
                End Sub

                Public Overrides Sub Handle(handler As Handler)
                    handler.OnValueChanged(Me)
                End Sub
            End Class

            Public Class Handler
                Public Overridable Sub OnButtonClicked([event] As ButtonClicked)
                End Sub

                Public Overridable Sub OnValueChanged([event] As ValueChanged)
                End Sub

                Public Shared ReadOnly Null As New Handler()
            End Class
        End Class

        Public Property CellEventHandler As RoutedCellEvent.Handler = RoutedCellEvent.Handler.Null

        Protected Overridable Sub OnCellEventHappend([event] As RoutedCellEvent)
            [event].Handle(CellEventHandler)
        End Sub

        Protected Overridable Sub RouteCellEvent([event] As RoutedCellEvent)
            Me.OnCellEventHappend([event])

            For Each child In Me.Children.Where(Function(e) e.ContainsCellAt([event].Cell.Location))
                child.RouteCellEvent([event])
            Next
        End Sub

        Public Overridable ReadOnly Property CellTypeFactory As CellTypeFactory
            Get
                Return Me.Parent.CellTypeFactory
            End Get
        End Property

        Public Sub New(parent As SheetElementViewModel, location As Point)
            Me._parent = parent
            Me._location = location
        End Sub

        Public Overridable Sub PasteTo(sheet As ISpreadSheet)
            For Each child In Me.Children
                child.PasteTo(sheet)
            Next
        End Sub
    End Class
End Namespace
