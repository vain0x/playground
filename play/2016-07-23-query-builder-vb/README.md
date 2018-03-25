---
tag: \#toy \#lib \#sql
---

# Query Builder

Thin SQL query builder for .NET

- **Pros**:
    - Less syntax error on runtime.
    - More readable queries.
- **Problems**:
    - No static type checking.
    - Use of undefined identifiers (table/column names).

## Example

This expression:

```vb
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
```

will produce an unformatted SQL string equal to:

```sql
select
    t0.x00,
    min(t0.x01) as m1
from table_zero as t0
    , table_one as t1
join table_two
    on 0 = 0
where ((t0.x0 = @p1))
group by t0.x00 , t0.x01
having ((t1.x1 = @p2))
order by t1.x10 , t1.x11 desc
```
