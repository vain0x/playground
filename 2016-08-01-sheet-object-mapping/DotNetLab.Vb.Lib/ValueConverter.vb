Namespace SheetObjectModel
    ''' <summary>
    ''' 値を内部表現と外部表現に相互変換するクラスのインターフェイスです。
    ''' </summary>
    Public Interface ValueConverter
        Function ToInternal(x As Object) As Object
        Function ToExternal(x As Object) As Object
    End Interface

    ''' <summary>
    ''' 何も変換しない ValueConverter クラスです。
    ''' </summary>
    Public Class IdentityConverter
        Implements ValueConverter

        Private Sub New()
        End Sub

        Public Function ToInternal(x As Object) As Object Implements ValueConverter.ToInternal
            Return x
        End Function

        Public Function ToExternal(x As Object) As Object Implements ValueConverter.ToExternal
            Return x
        End Function

        Private Shared _instance As IdentityConverter = New IdentityConverter
        Public Shared ReadOnly Property Instance As IdentityConverter
            Get
                Return _instance
            End Get
        End Property
    End Class
End Namespace
