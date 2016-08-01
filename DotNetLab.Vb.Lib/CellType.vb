Namespace SheetObjectModel
    Public MustInherit Class CellType
        Public Class Text
            Inherits CellType
        End Class

        Public Class Alphanumeric
            Inherits Text
        End Class

        Public Class Number
            Inherits CellType

            ''' <summary>
            ''' 小数点以下の桁数を表します。
            ''' </summary>
            Public Property DecimalPlaces As Integer = 0
        End Class

        Public Class [Integer]
            Inherits Number
        End Class

        Public Class [Double]
            Inherits Number

            Public Sub New()
                Me.DecimalPlaces = 2
            End Sub
        End Class

        Public Class DateTime
            Inherits CellType

            Private Const _dateTimeFormat = "yyyy/MM/dd HH:mm:ss"
            Public Property EditFormat As String = _dateTimeFormat
            Public Property DisplayFormat As String = _dateTimeFormat
        End Class

        Public Class [Date]
            Inherits DateTime

            Public Sub New()
                Me.EditFormat = DATE_FORMAT_YYYYMMDD
                Me.DisplayFormat = DATE_FORMAT_YYYYMMDD
            End Sub
        End Class

        Public Class JapaneseMonth
            Inherits DateTime

            Public Sub New()
                Me.EditFormat = DATE_FORMAT_YYYYMMDD
                Me.DisplayFormat = DATE_FORMAT_JAPANESE_MONTH
            End Sub
        End Class
    End Class

    ''' <summary>
    ''' セルの値の型から CelType を生成するファクトリークラスです。
    ''' </summary>
    Public MustInherit Class CellTypeFactory
        Public MustOverride Function CellTypeFromType(type As Type) As CellType

        Public Class [Default]
            Inherits CellTypeFactory

            Public Overrides Function CellTypeFromType(type As Type) As CellType
                Select Case type
                    Case GetType(String)
                        Return New CellType.Text()
                    Case GetType(Integer)
                        Return New CellType.Integer()
                    Case GetType(Double)
                        Return New CellType.Double()
                    Case GetType(DateTime)
                        Return New CellType.Date()
                    Case Else
                        Throw New NotSupportedException(String.Format("Can't convert '{0}' to 'SpreadObjectModel.Celltype'.", type.ToString()))
                End Select
            End Function

            Public Sub New()
            End Sub

            Public Shared ReadOnly Instance As New [Default]()
        End Class
    End Class
End Namespace
