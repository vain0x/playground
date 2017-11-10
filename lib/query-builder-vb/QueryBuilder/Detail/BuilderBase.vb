Public MustInherit Class BuilderBase(Of TDerived As {BuilderBase(Of TDerived)})
    Public Function ToStringOneline() As String
        Return Me.ToString().Replace(Environment.NewLine, " ").Trim()
    End Function

    Public Function ApplyTo(Of X)(f As Func(Of TDerived, X)) As X
        Return f(DirectCast(Me, TDerived))
    End Function
End Class
