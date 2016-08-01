Namespace SheetObjectModel
    Public Interface ICell
        ReadOnly Property Location As Point
        Property RowSpan As Integer
        Property ColumnSpan As Integer
        Property Locked As Boolean
        WriteOnly Property CellType As CellType
        Property Value As Object
    End Interface

    Public Interface ISpreadSheet
        ReadOnly Property Cells(iRow As Integer, iColumn As Integer) As ICell
        Property RowCount As Integer
    End Interface
End Namespace
