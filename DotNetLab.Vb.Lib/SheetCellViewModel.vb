Imports System.Drawing

Namespace SheetObjectModel
    ''' <summary>
    ''' スプレッドシートの1つのセルに対応するビューモデルを表します。
    ''' </summary>
    Public Class SheetCellViewModel(Of TValue)
        Inherits SheetElementViewModel

        Private ReadOnly _property As [Property](Of TValue)
        Private ReadOnly _cellTypeOrNull As CellType

        Public NotOverridable Overrides ReadOnly Iterator Property Children As IEnumerable(Of SheetElementViewModel)
            Get
            End Get
        End Property

        Private ReadOnly _size As Size

        Public NotOverridable Overrides ReadOnly Property Size As Size
            Get
                Return Me._size
            End Get
        End Property

        Private ReadOnly _valueConverter As ValueConverter

        Public Property Value As TValue
            Get
                Return DirectCast(Me._valueConverter.ToExternal(Me._property.Value), TValue)
            End Get
            Set(value As TValue)
                Me._property.Value = DirectCast(Me._valueConverter.ToInternal(value), TValue)
            End Set
        End Property

        Public NotOverridable Overrides Function ContainsCellAt(location As Point) As Boolean
            Dim x = location.X - Me.Location.X
            Dim y = location.Y - Me.Location.Y
            Return 0 <= x And x < Me.Size.Width _
                And 0 <= y And y < Me.Size.Height
        End Function

        Protected Overrides Sub RouteCellEvent([event] As SheetElementViewModel.RoutedCellEvent)
            Dim valueChangedEvent = TryCast([event], RoutedCellEvent.ValueChanged)
            If valueChangedEvent IsNot Nothing Then
                Me.Value = DirectCast(Convert.ChangeType(valueChangedEvent.NewValue, GetType(TValue)), TValue)
            End If
        End Sub

        Public Sub New([property] As [Property](Of TValue), parent As SheetElementViewModel, location As Point, size As Size?, valueConverter As ValueConverter, cellType As CellType)
            MyBase.New(parent, location)
            Me._property = [property]
            Me._size = If(size, New Size(1, 1))
            Me._valueConverter = If(valueConverter, IdentityConverter.Instance)
            Me._cellTypeOrNull = cellType
        End Sub

        Public Overrides Sub PasteTo(sheet As ISpreadSheet)
            Dim cell = sheet.Cells(Me.Location.Y, Me.Location.X)

            ' セルの型を指定します。
            cell.CellType = If(Me._cellTypeOrNull, Me.CellTypeFactory.CellTypeFromType(GetType(TValue)))

            ' セル連結を処理します。
            cell.RowSpan = Me.Size.Width
            cell.ColumnSpan = Me.Size.Height

            ' 編集不可ならロックします。
            cell.Locked = Not Me._property.CanWrite

            ' 値を設定します。
            cell.Value = Me.Value
        End Sub
    End Class

    Public Class SheetCellViewModel
        Public Shared Function Create(Of X)(
            [property] As [Property](Of X),
            parent As SheetElementViewModel,
            location As Point,
            Optional size As Size? = Nothing,
            Optional valueConverter As ValueConverter = Nothing,
            Optional cellType As CellType = Nothing
            ) As SheetCellViewModel(Of X)
            Return New SheetCellViewModel(Of X)([property], parent, location, size, valueConverter, cellType)
        End Function
    End Class
End Namespace
