namespace DotNetKit.Data.Entity

open System
open System.Collections
open System.Collections.Generic
open System.ComponentModel.DataAnnotations
open System.ComponentModel.DataAnnotations.Schema
open System.Data.Entity
open System.Linq
open System.Threading
open System.Threading.Tasks
open Persimmon
open Persimmon.Syntax.UseTestNameByReflection

[<AllowNullLiteral>]
type Student() =
  [<Key>]
  [<Column>]
  member val Id = 0L with get, set

  [<Column>]
  member val Name = (null: string) with get, set

  [<Column>]
  member val KlassId = 0L with get, set

and
  [<AllowNullLiteral>]
  Examination() =
  [<Key>]
  [<Column(Order = 0)>]
  member val StudentId = 0L with get, set

  [<Key>]
  [<Column(Order = 1)>]
  member val SubjectId = 0L with get, set

  [<Column>]
  member val Date = DateTime.MinValue with get, set

  [<Column>]
  member val Score = 0 with get, set

[<AutoOpen>]
module Misc =
  let studentOperation = 
    EntityOperation<Student>()

  let examinationOperation =
    EntityOperation<Examination>()

  let empty () =
    MemoryDbSchema()

  type SampleDatabase(schema, mikuId, lukaId, yukariId) =
    static let mathId = 1L
    static let scienceId = 2L
    static let japaneseId = 3L
    static let date = DateTime(2016, 01, 17)

    static member Seed(schema: MemoryDbSchema) =
      use context = schema.Connect()
      let students = context.Set<Student>()
      let miku = students.Add(Student(Name = "Miku"))
      let luka = students.Add(Student(Name = "Luka"))
      let yukari = students.Add(Student(Name = "Yukari"))
      context.SaveChanges() |> ignore
      let examinations = context.Set<Examination>()
      let examination studentId subjectId date score =
        Examination(StudentId = studentId, SubjectId = subjectId, Date = date, Score = score)
      examinations.Add(examination miku.Id mathId date 61) |> ignore
      examinations.Add(examination miku.Id scienceId date 62) |> ignore
      examinations.Add(examination luka.Id mathId date 71) |> ignore
      examinations.Add(examination luka.Id scienceId date 72) |> ignore
      context.SaveChanges() |> ignore
      (miku.Id, luka.Id, yukari.Id)

    new() =
      let schema = MemoryDbSchema()
      let (mikuId, lukaId, yukariId) = SampleDatabase.Seed(schema)
      SampleDatabase(schema, mikuId, lukaId, yukariId)

    new(d: SampleDatabase) =
      SampleDatabase(d.Schema, d.MikuId, d.LukaId, d.YukariId)

    member this.Schema = schema

    // Student IDs.
    member this.MikuId = mikuId
    member this.LukaId = lukaId
    member this.YukariId = yukariId

    // Subject IDs.
    member this.MathId = mathId
    member this.ScienceId = scienceId
    member this.JapaneseId = japaneseId

  type SingleTableContext<'entity when 'entity : not struct>(parent: SampleDatabase) =
    inherit SampleDatabase(parent)

    let context = parent.Schema.Connect()
    let set = context.Set<'entity>()

    member this.Context = context
    member this.Set = set
    member this.Add(entity) = set.Add(entity) |> ignore
    member this.Find([<ParamArray>] keyValues) = set.Find(keyValues)
    member this.SaveChanges() = context.SaveChanges() |> ignore

    member this.Reconnect() =
      new SingleTableContext<'entity>(this)

    member this.Dispose() = context.Dispose()

    interface IDisposable with
      override this.Dispose() = this.Dispose()

module ``test EntityOperation`` =
  let ``test Create`` =
    test {
      let student = studentOperation.Create()
      do! student.GetType() |> assertEquals typeof<Student>
    }

  let ``test Clone`` =
    test {
      let student = Student(Id = 1L, Name = "student name")
      let clone = studentOperation.Clone(student)
      do! clone.Id |> assertEquals student.Id
      do! clone.Name |> assertEquals student.Name
      do! obj.ReferenceEquals(student, clone) |> assertEquals false
    }

  let ``test KeyValues`` =
    let body (entity, expected) =
      test {
        let operation = EntityOperationModule.Create(entity.GetType())
        let actual = operation.KeyValues(entity)
        do! actual |> assertEquals expected
      }
    parameterize {
      case (Student(Id = 1L) :> obj, [|1L :> obj|])
      case (Examination(StudentId = 1L, SubjectId = 2L) :> obj, [|1L; 2L|])
      run body
    }

  let ``test NonkeyValues`` =
    let body (entity, expected) =
      test {
        let operation = EntityOperationModule.Create(entity.GetType())
        let actual = operation.NonkeyValues(entity)
        do! actual |> assertEquals expected
      }
    parameterize {
      case (Student(Name = "name", KlassId = 1L) :> obj, [|"name" :> obj; 1L :> obj|])
      case
        ( Examination(Date = DateTime(2016, 01, 02), Score = 61) :> obj
        , [| DateTime(2016, 01, 02); 61 |]
        )
      run body
    }

  module ``test IsSurrogateKeyed`` =
    type NaturalIdKeyed() =
      [<Key>]
      [<Column>]
      [<DatabaseGenerated(DatabaseGeneratedOption.None)>]
      member val Id = 0L with get, set

    let test =
      test {
        do! studentOperation.IsSurrogateKeyed |> assertEquals true

        // Multicolumn key is not a surrogate key.
        do! examinationOperation.IsSurrogateKeyed |> assertEquals false

        // Non-identity key is not a surrogate key.
        do! EntityOperation<NaturalIdKeyed>().IsSurrogateKeyed |> assertEquals false
      }

  let ``test GetOrSetSurrogateKey generated case`` =
    test {
      let student = Student(Id = 0L)
      do! studentOperation.GetOrSetSurrogateKey(student, fun () -> 2L) |> assertEquals 2L
      do! student.Id |> assertEquals 2L
    }

  let ``test GetOrSetSurrogateKey not-generated case`` =
    test {
      let student = Student(Id = 1L)
      do! studentOperation.GetOrSetSurrogateKey(student, fun () -> 2L) |> assertEquals 1L
      do! student.Id |> assertEquals 1L
    }

module ``test MemoryDbSet`` =
  let context () =
    new SingleTableContext<Student>(SampleDatabase())
    
  let ``test AddRange`` =
    test {
      use context = context ()
      let meiko = Student(Name = "Meiko")
      let kaito = Student(Name = "Kaito")
      context.Set.AddRange([|meiko; kaito|]) |> ignore
      context.SaveChanges()
      do! context.Set |> Seq.toArray |> Seq.length |> assertEquals 5
    }

  let ``test AddRange collects entities`` =
    test {
      use context = context ()
      let meiko = Student(Name = "Meiko")
      let kaito = Student(Name = "Kaito")
      do!
        context.Set.AddRange([|meiko; kaito|])
        |> Seq.toArray
        |> assertEquals [|meiko; kaito|]
    }

  let ``test RemoveRange`` =
    test {
      use context = context ()
      let miku = context.Find(context.MikuId)
      let luka = context.Find(context.LukaId)
      context.Set.RemoveRange([|miku; luka|]) |> ignore
      context.SaveChanges()
      do! context.Set |> Seq.toArray |> Seq.length |> assertEquals 1
    }

  let ``test RemoveRange collects entities`` =
    test {
      use context = context ()
      let miku = context.Find(context.MikuId)
      let luka = context.Find(context.LukaId)
      do!
        context.Set.RemoveRange([|miku; luka|])
        |> Seq.toArray
        |> assertEquals [|miku; luka|]
    }

  module ``test asynchronous operations`` =
    let ``test non-enumeration`` =
      test {
        use context = context ()
        let count = context.Set.CountAsync().Result
        do! count |> assertEquals 3
      }

    let ``test non-enumeration cancellation`` =
      test {
        use context = context ()
        let cancellationTokenSource = new CancellationTokenSource()
        cancellationTokenSource.Cancel()
        let start () = context.Set.CountAsync(cancellationTokenSource.Token)
        let! e = trap { it ((start ()).Wait()) }
        do! e :? OperationCanceledException |> assertEquals true
      }

    let ``test enumeration`` =
      test {
        use context = context ()
        let students = context.Set.ToArrayAsync().Result
        do!
          students
          |> Seq.map (fun student -> student.Name)
          |> Seq.toArray
          |> assertEquals [|"Miku"; "Luka"; "Yukari"|]
      }

    let ``test enumeration cancellation`` =
      test {
        use context = context ()
        let cancellationTokenSource = new CancellationTokenSource()
        cancellationTokenSource.Cancel()
        let start () = context.Set.ToArrayAsync(cancellationTokenSource.Token)
        let! e = trap { it ((start ()).Wait()) }
        let innerException =
          (e :?> AggregateException).InnerExceptions
          |> Seq.exactlyOne
        do! innerException :? TaskCanceledException |> assertEquals true
      }

    let ``test operator`` =
      test {
        use context = context ()
        let students =
          context.Set
            .Where(fun student -> student.Name = "Miku")
            .Select(fun student -> student.Name)
            .ToArrayAsync()
            .Result
        do! students |> assertEquals [|"Miku"|]
      }

module ``test NaturalKeyedMemoryDbSet`` =
  let context () =
    new SingleTableContext<Examination>(SampleDatabase())

  let ``test Find`` =
    test {
      use context = context ()
      let entity = context.Find(context.MikuId, context.MathId)
      do! entity.Score |> assertEquals 61
    }

  let ``test Find doesn't find unexisting entities`` =
    test {
      use context = context ()
      do! context.Find(context.MikuId, context.JapaneseId) |> assertEquals null
    }

  let ``test Find doesn't find entities being removed`` =
    test {
      use context = context ()
      let entity = context.Find(context.MikuId, context.MathId)
      context.Set.Remove(entity) |> ignore
      do! context.Find(context.MikuId, context.MathId) |> assertEquals null
    }

  let ``test Add`` =
    test {
      use context = context ()
      let (mikuId, japaneseId) = (context.MikuId, context.JapaneseId)
      let score = 81
      let entity = Examination(StudentId = mikuId, SubjectId = japaneseId, Score = score)
      context.Add(entity)
      context.SaveChanges()
      do! (entity.StudentId, entity.SubjectId) |> assertEquals (mikuId, japaneseId)

      use context = context.Reconnect()
      let entity = context.Find(mikuId, japaneseId)
      do! entity.Score |> assertEquals score
    }

  let ``test Add can conflict with an attached entity`` =
    test {
      use context = context ()
      let (mikuId, japaneseId) = (context.MikuId, context.JapaneseId)
      let entity1 = Examination(StudentId = mikuId, SubjectId = japaneseId, Score = 63)
      let entity2 = Examination(StudentId = mikuId, SubjectId = japaneseId, Score = 99)
      context.Add(entity1)
      let! e = trap { it (context.Add(entity2)) }
      return ()
    }

  let ``test Add can conflict with a detached entity`` =
    test {
      use context = context ()
      let entity = Examination(StudentId = context.MikuId, SubjectId = context.MathId)
      context.Add(entity)
      let! e = trap { it (context.SaveChanges()) }
      return ()
    }

  let ``test Remove can remove changing entity`` =
    test {
      use context = context ()
      let entity = context.Find(context.MikuId, context.ScienceId)
      context.Set.Remove(entity) |> ignore
      context.SaveChanges()

      use context = context.Reconnect()
      do! context.Find(context.MikuId, context.ScienceId) |> assertEquals null
    }

  let ``test Remove can prevent entities being added`` =
    test {
      use context = context ()
      let entity = Examination(StudentId = context.MikuId, SubjectId = context.JapaneseId, Score = 63)
      context.Add(entity)
      context.Set.Remove(entity) |> ignore
      context.SaveChanges()

      use context = context.Reconnect()
      do! context.Find(context.MikuId, context.JapaneseId) |> assertEquals null
    }

  let ``test Remove can cause an exception if unexisting entity is removed`` =
    test {
      use context = context ()
      let entity = Examination(StudentId = context.MikuId, SubjectId = context.JapaneseId)
      context.Set.Remove(entity) |> ignore
      let! e = trap { it (context.SaveChanges()) }
      return ()
    }

  let ``test Update`` =
    test {
      use context = context ()
      let entity = context.Find(context.MikuId, context.ScienceId)
      let newScore = entity.Score + 1
      entity.Score <- newScore
      context.SaveChanges()

      use context = context.Reconnect()
      let entity = context.Find(context.MikuId, context.ScienceId)
      do! entity.Score |> assertEquals newScore
    }

  let ``test GetEnumerator`` =
    test {
      use context = context ()
      let entity1 = Examination(StudentId = context.MikuId, SubjectId = context.JapaneseId, Score = 63)
      context.Add(entity1)
      let entity2 = context.Find(context.MikuId, context.MathId)
      do!
        query {
          for entity in context.Set do
          select entity.Score
        }
        |> Seq.sort
        |> Seq.toArray
        |> assertEquals [|61; 62; 63; 71; 72|]
    }

  let ``test GetEnumerator doesn't yield entities being removed`` =
    test {
      use context = context ()
      let entity = context.Find(context.MikuId, context.MathId)
      context.Set.Remove(entity) |> ignore
      do!
        context.Set |> Seq.map (fun entity -> entity.Score) |> Seq.sort |> Seq.toArray
        |> assertEquals [|62; 71; 72|]
    }

module ``test SurrogateKeyedMemoryDbSet`` =
  let context () =
    new SingleTableContext<Student>(SampleDatabase())

  let ``test Find`` =
    test {
      use context = context ()
      let student = Student(Name = "Kaito")
      context.Add(student)
      context.SaveChanges()

      use context = context.Reconnect()
      let student = context.Find(student.Id)
      do! student.Name |> assertEquals "Kaito"
    }

  let ``test Find not found`` =
    test {
      use context = context ()
      do! context.Find(0L) |> assertEquals null
    }

  let ``test Find doesn't find entities being removed`` =
    test {
      use context = context ()
      let miku = context.Find(context.MikuId)
      context.Set.Remove(miku) |> ignore
      do! context.Find(context.MikuId) |> assertEquals null
    }

  let ``test Add generates Id`` =
    test {
      use context = context ()
      let student = Student(Name = "Kaito")
      context.Add(student)
      do! EntityOperation<Student>.IsValidSurrogateKey(student.Id) |> assertEquals false
      context.SaveChanges()
      do! EntityOperation<Student>.IsValidSurrogateKey(student.Id) |> assertEquals true
    }

  let ``test Add doesn't override Id`` =
    test {
      use context = context ()
      let student = Student(Id = 999L, Name = "Kaito")
      context.Add(student)
      context.SaveChanges()
      do! student.Id |> assertEquals 999L
    }

  let ``test Remove`` =
    test {
      use context = context ()
      let student = context.Find(context.MikuId)
      context.Set.Remove(student) |> ignore
      context.SaveChanges()

      use context = context.Reconnect()
      do! context.Find(context.MikuId) |> assertEquals null
      return ()
    }

  let ``test GetEnumerator`` =
    test {
      use context = context ()
      context.Add(Student(Name = "Kaito"))
      let yukari = context.Find(context.YukariId)
      yukari.Name <- "Yukari Rin"
      do!
        query {
          for student in context.Set do
          select student.Name
        }
        |> Seq.sort
        |> Seq.toArray
        |> assertEquals [|"Kaito"; "Luka"; "Miku"; "Yukari Rin"|]
    }

  let ``test GetEnumerator doesn't yield entities being removed`` =
    test {
      use context = context ()
      let miku = context.Find(context.MikuId)
      context.Set.Remove(miku) |> ignore
      do!
        context.Set |> Seq.map (fun student -> student.Name) |> Seq.sort |> Seq.toArray
        |> assertEquals [|"Luka"; "Yukari"|]
    }
