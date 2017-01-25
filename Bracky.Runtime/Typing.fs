namespace Bracky.Runtime.Typing

open System

module private Map =
  let addMany kvs this =
    kvs |> Seq.fold (fun this (k, v) -> this |> Map.add k v) this

[<RequireQualifiedAccess>]
type Kind =
  | Unit
  | Int
  | Bool

type TypeVariable =
  | TypeVariable of int64
with
  override this.ToString() =
    let (TypeVariable identifier) = this
    sprintf "'%d" identifier

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TypeVariable =
  let count = ref 0L
  let fresh () =
    let value = !count
    count := value + 1L
    value |> TypeVariable

type TypeExpression =
  | RefTypeExpression
    of TypeVariable
  | FunTypeExpression
    of TypeExpression * TypeExpression
  | AppTypeExpression
    of Kind * array<TypeExpression>
with
  override this.ToString() =
    match this with
    | RefTypeExpression tv ->
      string tv
    | FunTypeExpression (sourceType, targetType) ->
      sprintf "(%s -> %s)" (string sourceType) (string targetType)
    | AppTypeExpression (kind, arguments) ->
      if arguments |> Array.isEmpty then
        string kind
      else
        let argumentList = arguments |> Array.map string |> String.concat " "
        sprintf "(%s %s)" (string kind) argumentList

  member this.TypeVariableSet =
    match this with
    | RefTypeExpression tv ->
      Set.singleton tv
    | FunTypeExpression (s, t) ->
      Set.union s.TypeVariableSet t.TypeVariableSet
    | AppTypeExpression (_, arguments) ->
      arguments |> Seq.map (fun t -> t.TypeVariableSet) |> Set.unionMany

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TypeExpression =
  let unit =
    AppTypeExpression (Kind.Unit, [||])

  let int =
    AppTypeExpression (Kind.Int, [||])

  let bool =
    AppTypeExpression (Kind.Bool, [||])

[<Sealed>]
type Substitution private (map: Map<_, _>) =
  let apply =
    let lookup tv =
      match map |> Map.tryFind tv with
      | Some t -> t
      | None -> RefTypeExpression tv
    let rec apply t =
      match t with
      | RefTypeExpression tv ->
        let t' = lookup tv
        if t = t' then t else apply t'
      | FunTypeExpression (s, t) ->
        FunTypeExpression (apply s, apply t)
      | AppTypeExpression (kind, arguments) ->
        AppTypeExpression (kind, arguments |> Array.map apply)
    apply

  member this.Apply(t: TypeExpression) =
    apply t

  member this.Extend(tu: TypeVariable, t: TypeExpression) =
    if apply t = RefTypeExpression tu then
      this
    else
      Substitution(map |> Map.add tu t)

  member this.ExtendMany(bindings: array<TypeVariable * TypeExpression>) =
    let bindings =
      bindings |> Array.filter
        (fun (tv, t) -> apply t <> RefTypeExpression tv)
    if bindings.Length = 0 then
      this
    else
      Substitution(map |> Map.addMany bindings)

  static member val Empty =
    Substitution(Map.empty)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Substitution =
  /// Unifies two type expressions
  /// to update the substitution so that it equalizes them.
  let rec unify t t' (this: Substitution) =
    match (this.Apply(t), this.Apply(t')) with
    | (RefTypeExpression tv, RefTypeExpression tv') when tv = tv' ->
      this.Extend(tv, t')
    | (RefTypeExpression tv, _)
      when (t': TypeExpression).TypeVariableSet |> Set.contains tv |> not ->
      this.Extend(tv, t')
    | (_, RefTypeExpression tv) ->
      unify t' t this
    | (FunTypeExpression (s, u), FunTypeExpression (s', u')) ->
      this
      |> unify u u'
      |> unify s s'
    | (AppTypeExpression (kind, ts), AppTypeExpression (kind', ts'))
      when kind = kind' && ts.Length = ts'.Length ->
      Array.zip ts ts' |> Array.fold
        (fun this (u, u') -> unify u u' this)
        this
    | (_, _) ->
      failwith "TODO: error handling"

type ForallTypeScheme =
  | ForallTypeScheme
    of array<TypeVariable> * TypeExpression
with
  member this.Instantiate() =
    let (ForallTypeScheme (tvs, t)) = this
    let bindings =
      tvs |> Array.map
        (fun tv -> (tv, TypeVariable.fresh () |> RefTypeExpression))
    let substitution =
      Substitution.Empty.ExtendMany(bindings)
    substitution.Apply(t)

  member this.FreeTypeVariableSet =
    let (ForallTypeScheme (tvs, t)) = this
    Set.difference t.TypeVariableSet (tvs |> Set.ofArray)

/// From variables to type schemes.
[<Sealed>]
type TypeEnvironment private (map: Map<string, ForallTypeScheme>) =
  let freeTypeVariableSet =
    lazy
      map |> Map.fold
        (fun set _ ts -> Set.union set ts.FreeTypeVariableSet)
        Set.empty

  /// Converts a type expression to a type scheme by binding all free variables with ∀.
  let generalize t =
    let tvs =
      Set.difference (t: TypeExpression).TypeVariableSet freeTypeVariableSet.Value
      |> Set.toArray
    ForallTypeScheme (tvs, t)

  member this.TryFind(identifier) =
    map |> Map.tryFind identifier

  member this.Add(identifier, ts) =
    TypeEnvironment(map |> Map.add identifier ts)

  member this.Generalize(t) =
    generalize t

  static member val Empty =
    TypeEnvironment(Map.empty)

type TypeInferer =
  internal
    {
      Substitution:
        Substitution
      TypeEnvironment:
        TypeEnvironment
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TypeInferer =
  open Bracky.Runtime.Parsing

  let empty =
    {
      Substitution =
        Substitution.Empty
      TypeEnvironment =
        TypeEnvironment.Empty
    }

  let substitution (this: TypeInferer) =
    this.Substitution

  let typeEnvironment (this: TypeInferer) =
    this.TypeEnvironment

  let bind tv t (this: TypeInferer) =
    { this with Substitution = this.Substitution.Extend(tv, t) }

  let conclude identifier typeScheme (this: TypeInferer) =
    { this with TypeEnvironment = this.TypeEnvironment.Add(identifier, typeScheme) }

  let local f (this: TypeInferer) =
    let inferer = this |> f
    { inferer with TypeEnvironment = this.TypeEnvironment }

  let unify t t' (this: TypeInferer) =
    { this with Substitution = this.Substitution |> Substitution.unify t t' }

  /// Infers the type of the expression `x` under the given substitution and environment
  /// and returns an extended substitution and an environment which describes type of each variable.
  let infer =
    let rec infer previous expression t this =
      let infer = infer (Some expression)
      match expression with
      | IntExpression _ ->
        this |> unify t TypeExpression.int
      | BoolExpression _ ->
        this |> unify t TypeExpression.bool
      | RefExpression (_, identifier) ->
        match this.TypeEnvironment.TryFind(identifier) with
        | Some (typeScheme: ForallTypeScheme) ->
          let t' = typeScheme.Instantiate()
          this |> unify t t'
        | None ->
          failwith "TODO: variable not defined"
      | FunExpression (_, pattern, expression) ->
        let (IdentifierPattern (_, identifier)) = pattern
        let tv = TypeVariable.fresh () |> RefTypeExpression
        let tu = TypeVariable.fresh () |> RefTypeExpression
        let t' = FunTypeExpression (tv, tu)
        this
        |> unify t t'
        |> local
            (fun this ->
              this
              |> conclude identifier (ForallTypeScheme ([||], tv))
              |> infer expression tu
            )
      | IfExpression (headClause, tailClauses) ->
        let (this, elseExists) =
          Array.append [|headClause|] tailClauses |> Array.fold
            (fun (this, elseExists) clause ->
              match clause with
              | IfClause (condition, expression) ->
                let this =
                  this
                  |> infer condition TypeExpression.bool
                  |> infer expression t
                (this, elseExists)
              | ElseClause expression ->
                (this |> infer expression t, true)
            ) (this, false)
        if elseExists
        then this
        else this |> unify t TypeExpression.unit
      | BinaryOperationExpression (operator, left, right) ->
        match operator with
        | ApplyOperator ->
          let tv = TypeVariable.fresh () |> RefTypeExpression
          this
          |> infer left (FunTypeExpression (tv, t))
          |> infer right t
        | ThenOperator ->
          this
          |> infer left TypeExpression.unit
          |> infer right t
        | AddOperator
        | MulOperator ->
          this
          |> infer left TypeExpression.int
          |> infer right TypeExpression.int
          |> unify t TypeExpression.int
      | ValExpression ((IdentifierPattern (_, identifier)), expression) ->
        let tv = TypeVariable.fresh () |> RefTypeExpression
        this
        |> unify t TypeExpression.unit
        |> infer expression tv
        |>  (fun this ->
              let t = this.TypeEnvironment.Generalize(this.Substitution.Apply(tv))
              this |> conclude identifier t
            )
    fun expression t this ->
      this |> infer None expression t
