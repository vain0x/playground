Imports System.Text.RegularExpressions
Imports System.Threading
Imports Xunit

Public Class SelectBuilderTest
    Public ReadOnly _q As New QueryBuilder(New FakeDbParameterFactory())

    Private Function NumberParameters(sql As String) As String
        Dim i = 0
        Return Regex.Replace(
            sql, "@__\w+",
            Function(m) String.Format("@p{0}", Interlocked.Increment(i)))
    End Function

    <Fact>
    Public Sub NumberParametersTest()
        Assert.Equal("x in (@p1, @p2)", Me.NumberParameters("x in (@__ab0, @__c1d)"))
    End Sub

    <Fact>
    Public Sub MinimumTest()
        Dim query = Me._q.Select().From("t")
        Assert.Equal("select * from t", query.ToStringOneline())
    End Sub

    <Fact>
    Public Sub FullTest()
        Dim sql =
            Me._q.Select() _
            .Field("t0.x00") _
            .Field("min(t0.x01)", "m1") _
            .From("table_zero", "t0") _
            .From("table_one", "t1") _
            .JoinOn("table_two", Me._q.And()) _
            .Where(Me._q.And().Equal("t0.x0", "a0")) _
            .GroupBy("t0.x00") _
            .GroupBy("t0.x01") _
            .Having(Me._q.And().Equal("t1.x1", "a1")) _
            .OrderBy("t1.x10") _
            .OrderByDesc("t1.x11") _
            .ApplyTo(Function(q) NumberParameters(q.ToStringOneline()))
        Dim expected = "select t0.x00 , min(t0.x01) as m1 from table_zero as t0 , table_one as t1 join table_two on 0 = 0 where ((t0.x0 = @p1)) group by t0.x00 , t0.x01 having ((t1.x1 = @p2)) order by t1.x10 , t1.x11 desc"
        Assert.Equal(expected, sql)
    End Sub
End Class
