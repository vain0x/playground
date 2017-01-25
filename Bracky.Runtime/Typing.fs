namespace Bracky.Runtime.Typing

open System
open Bracky.Runtime.Parsing

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

[<AbstractClass>]
type Substitution() =
  abstract Item: TypeVariable -> TypeExpression with get

  member this.Apply(t: TypeExpression) =
    match t with
    | RefTypeExpression tv ->
      let t' = this.[tv]
      if t = t' then t else this.Apply(t')
    | FunTypeExpression (s, t) ->
      FunTypeExpression (this.Apply(s), this.Apply(t))
    | AppTypeExpression (kind, arguments) ->
      AppTypeExpression (kind, arguments |> Array.map this.Apply)

  member this.Extend(tu: TypeVariable, t: TypeExpression) =
    if this.Apply(t) = RefTypeExpression tu then
      this
    else
      let that = this
      { new Substitution() with
          override this.Item
            with get tv =
              if tv = tu then t else that.[tv]
      }

  member this.ExtendMany(bindings: Map<TypeVariable, TypeExpression>) =
    let bindings =
      bindings |> Map.filter (fun tv t -> this.Apply(t) <> RefTypeExpression tv)
    if bindings.Count = 0 then
      this
    else
      let that = this
      { new Substitution() with
          override this.Item
            with get tv =
              match bindings |> Map.tryFind tv with
              | Some t -> t
              | None -> that.[tv]
      }

  static member val Empty =
    { new Substitution() with
        override this.Item
          with get tv = RefTypeExpression tv
    }

type ForallTypeScheme =
  | ForallTypeScheme
    of array<TypeVariable> * TypeExpression
with
  member this.Instantiate() =
    let (ForallTypeScheme (tvs, t)) = this
    let bindings =
      tvs
      |> Array.map (fun tv -> (tv, TypeVariable.fresh () |> RefTypeExpression))
      |> Map.ofArray
    let substitution =
      Substitution.Empty.ExtendMany(bindings)
    substitution.Apply(t)

  member this.FreeTypeVariableSet =
    let (ForallTypeScheme (tvs, t)) = this
    Set.difference t.TypeVariableSet (tvs |> Set.ofArray)

/// From variables to type schemes.
type TypeEnvironment =
  Map<string, ForallTypeScheme>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TypeEnvironment =
  // not used
  let freeTypeVariableSet (this: TypeEnvironment) =
    this
    |> Seq.map (fun (KeyValue (_, ts)) -> ts.FreeTypeVariableSet)
    |> Set.unionMany

  let variableSet (this: TypeEnvironment) =
    this |> Seq.map (fun (KeyValue (k, v)) -> k) |> Set.ofSeq

  /// Converts a type expression to a type scheme by binding all free variables with ∀.
  let generalize t (this: TypeEnvironment) =
    let tvs = (t: TypeExpression).TypeVariableSet |> Set.toArray
    ForallTypeScheme (tvs, t)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TypeInference =
  open Bracky.Runtime.Parsing

  /// Unifies two type expressions under the given substitution
  /// and returns an extended substitution which equalize them.
  let rec unify t t' (substitution: Substitution) =
    match (substitution.Apply(t), substitution.Apply(t')) with
    | (RefTypeExpression tv, RefTypeExpression tv') when tv = tv' ->
      substitution.Extend(tv, t')
    | (RefTypeExpression tv, _)
      when (t': TypeExpression).TypeVariableSet |> Set.contains tv |> not ->
      substitution.Extend(tv, t')
    | (_, RefTypeExpression tv) ->
      unify t' t substitution
    | (FunTypeExpression (s, u), FunTypeExpression (s', u')) ->
      substitution
      |> unify u u'
      |> unify s s'
    | (AppTypeExpression (kind, arguments), AppTypeExpression (kind', arguments'))
      when kind = kind' && arguments.Length = arguments'.Length ->
      Array.zip arguments arguments' |> Array.fold
        (fun substitution (a, a') ->
          unify a a' substitution
        ) substitution
    | (_, _) ->
      failwith "TODO: error handling"

  /// Infers the type of the expression `x` under the given substitution and environment
  /// and returns an extended substitution and an environment which describes type of each variable.
  let infer =
    let rec loop previous expression t (substitution, environment) =
      let loop = loop (Some expression)
      match expression with
      | IntExpression _ ->
        let substitution = substitution |> unify t TypeExpression.int
        (substitution, environment)
      | BoolExpression _ ->
        let substitution = substitution |> unify t TypeExpression.bool
        (substitution, environment)
      | RefExpression (_, identifier) ->
        match environment |> Map.tryFind identifier with
        | Some (typeScheme: ForallTypeScheme) ->
          let t' = typeScheme.Instantiate()
          let substitution = unify t t' substitution
          (substitution, environment)
        | None ->
          failwith "TODO: variable not defined"
      | FunExpression (_, pattern, expression) ->
        let (IdentifierPattern (_, identifier)) = pattern
        let tv = TypeVariable.fresh () |> RefTypeExpression
        let tu = TypeVariable.fresh () |> RefTypeExpression
        let t' = FunTypeExpression (tv, tu)
        let substitution = substitution |> unify t t'
        let environment = environment |> Map.add identifier (ForallTypeScheme ([||], tv))
        loop expression tu (substitution, environment)
      | IfExpression _ ->
        NotImplementedException() |> raise
      | BinaryOperationExpression (operator, left, right) ->
        match operator with
        | ApplyOperator ->
          let tv = TypeVariable.fresh () |> RefTypeExpression
          let tu = TypeVariable.fresh () |> RefTypeExpression
          let (substitution, environment) =
            (substitution, environment)
            |> loop left (FunTypeExpression (tv, tu))
            |> loop right tu
          let substitution =
            substitution |> unify t tu
          (substitution, environment)
        | ThenOperator ->
          (substitution, environment)
          |> loop left TypeExpression.unit
          |> loop right t
        | _ ->
          NotImplementedException() |> raise
      | ValExpression ((IdentifierPattern (_, identifier)), expression) ->
        let tv = TypeVariable.fresh () |> RefTypeExpression
        let (substitution, environment) =
          (substitution, environment) |> loop expression tv
        let variableType = environment |> TypeEnvironment.generalize (substitution.Apply(tv))
        let environment = environment |> Map.add identifier variableType
        let substitution = substitution |> unify t TypeExpression.unit
        (substitution, environment)
    fun expression t substitution environment ->
      loop None expression t (substitution, environment)
