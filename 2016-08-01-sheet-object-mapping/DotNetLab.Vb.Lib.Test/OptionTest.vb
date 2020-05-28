Imports Xunit

Public Class OptionTest
    <Fact>
    Public Sub ValueOrTest()
        Assert.Equal(1, [Option].Some(1).ValueOr(2))
        Assert.Equal(1, [Option].None(Of Integer)().ValueOr(1))
    End Sub

    <Fact>
    Public Sub ValueOrElseTest()
        Assert.Equal(
            1,
            [Option].Some(1).ValueOrElse(
                Function()
                    ' Shouldn't be evaluated.
                    Throw New Exception()
                End Function))
        Assert.Equal(1, [Option].None(Of Integer)().ValueOrElse(Function() 1))
    End Sub

    <Fact>
    Public Sub IEnumerableTest()
        Assert.Equal({1}, [Option].Some(1).ToArray())
        Assert.Equal({"abc"}, [Option].Some("abc").ToArray())
        Assert.Equal({}, [Option].None(Of Integer)().ToArray())
        Assert.Equal(New String() {}, [Option].None(Of String)().ToArray())
    End Sub

    <Fact>
    Public Sub MapTest()
        Assert.Equal({1}, [Option].Some(0).Map(Function(x) x + 1).ToArray())
        Assert.Equal({}, [Option].None(Of Integer).Map(Function(x) x + 1).ToArray())
    End Sub

    <Fact>
    Public Sub BindTest()
        Dim f As Func(Of Integer, [Option](Of Integer)) = Function(x) [Option].Some(x + 1)
        Assert.Equal({1}, [Option].Some(0).Bind(f).ToArray())
        Assert.Equal({}, [Option].None(Of Integer)().Bind(f).ToArray())
        Assert.Equal({}, [Option].Some(0).Bind(Function(x) [Option].None(Of Integer)()).ToArray())
    End Sub
End Class
