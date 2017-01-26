namespace Bracky.Runtime.Typing

open System

module private Map =
  let addMany kvs map =
    kvs |> Seq.fold (fun this (k, v) -> this |> Map.add k v) map

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
  | VarTypeExpression
    of TypeVariable
  | FunTypeExpression
    of TypeExpression * TypeExpression
  | KindTypeExpression
    of Kind * array<TypeExpression>
with
  override this.ToString() =
    match this with
    | VarTypeExpression tv ->
      string tv
    | FunTypeExpression (sourceType, targetType) ->
      sprintf "(%s -> %s)" (string sourceType) (string targetType)
    | KindTypeExpression (kind, arguments) ->
      if arguments |> Array.isEmpty then
        string kind
      else
        let argumentList = arguments |> Array.map string |> String.concat " "
        sprintf "(%s %s)" (string kind) argumentList

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TypeExpression =
  let unit =
    KindTypeExpression (Kind.Unit, [||])

  let int =
    KindTypeExpression (Kind.Int, [||])

  let bool =
    KindTypeExpression (Kind.Bool, [||])

  let rec typeVariables t =
    seq {
      match t with
      | VarTypeExpression tv ->
        yield tv
      | FunTypeExpression (s, u) ->
        yield! typeVariables s
        yield! typeVariables u
      | KindTypeExpression (_, ts) ->
        for u in ts do
          yield! typeVariables u
    }

[<Sealed>]
type Substitution private (map: Map<_, _>) =
  let apply =
    let lookup tv =
      match map |> Map.tryFind tv with
      | Some t -> t
      | None -> VarTypeExpression tv
    let rec apply t =
      match t with
      | VarTypeExpression tv ->
        let t' = lookup tv
        if t = t' then t else apply t'
      | FunTypeExpression (s, t) ->
        FunTypeExpression (apply s, apply t)
      | KindTypeExpression (kind, arguments) ->
        KindTypeExpression (kind, arguments |> Array.map apply)
    apply

  member this.Apply(t: TypeExpression) =
    apply t

  member this.Extend(tu: TypeVariable, t: TypeExpression) =
    if apply t = VarTypeExpression tu then
      this
    else
      Substitution(map |> Map.add tu t)

  member this.ExtendMany(bindings: array<TypeVariable * TypeExpression>) =
    let bindings =
      bindings |> Array.filter
        (fun (tv, t) -> apply t <> VarTypeExpression tv)
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
    | (VarTypeExpression tv, VarTypeExpression tv') when tv = tv' ->
      this.Extend(tv, t')
    | (VarTypeExpression tv, _)
      when t' |> TypeExpression.typeVariables |> Seq.forall ((<>) tv) ->
      this.Extend(tv, t')
    | (_, VarTypeExpression tv) ->
      unify t' t this
    | (FunTypeExpression (s, u), FunTypeExpression (s', u')) ->
      this
      |> unify u u'
      |> unify s s'
    | (KindTypeExpression (kind, ts), KindTypeExpression (kind', ts'))
      when kind = kind' && ts.Length = ts'.Length ->
      Array.zip ts ts' |> Array.fold
        (fun this (u, u') -> unify u u' this)
        this
    | (_, _) ->
      failwith "TODO: unification error"

type TypeScheme =
  | TypeScheme
    of array<TypeVariable> * TypeExpression
with
  member this.Instantiate() =
    let (TypeScheme (tvs, t)) = this
    let bindings =
      tvs |> Array.map
        (fun tv -> (tv, TypeVariable.fresh () |> VarTypeExpression))
    let substitution =
      Substitution.Empty.ExtendMany(bindings)
    substitution.Apply(t)

  member this.FreeTypeVariableSet =
    let (TypeScheme (tvs, t)) = this
    Set.difference (TypeExpression.typeVariables t |> Set.ofSeq) (tvs |> Set.ofArray)

/// From variables to type schemes.
[<Sealed>]
type TypeEnvironment private (map: Map<string, TypeScheme>) =
  /// Converts a type expression to a type scheme by binding all free variables with ∀.
  let generalize t =
    let freeTypeVariableSet =
      map |> Map.fold
        (fun set _ ts -> Set.union set ts.FreeTypeVariableSet)
        Set.empty
    let tvs =
      t |> TypeExpression.typeVariables
      |> Seq.filter (fun tv -> freeTypeVariableSet |> Set.contains tv |> not)
      |> Seq.toArray
    TypeScheme (tvs, t)

  member this.TryFind(identifier) =
    map |> Map.tryFind identifier

  member this.Add(identifier, ts) =
    TypeEnvironment(map |> Map.add identifier ts)

  member this.Generalize(t) =
    generalize t

  static member val Empty =
    TypeEnvironment(Map.empty)

type VariableTypeMap =
  Map<int64, TypeScheme>

type TypeInferer =
  internal
    {
      Substitution:
        Substitution
      TypeEnvironment:
        TypeEnvironment
      VariableTypeMap:
        VariableTypeMap
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
      VariableTypeMap =
        Map.empty
    }

  let substitution (inferer: TypeInferer) =
    inferer.Substitution

  let typeEnvironment (inferer: TypeInferer) =
    inferer.TypeEnvironment

  let bind tv t (inferer: TypeInferer) =
    { inferer with Substitution = inferer.Substitution.Extend(tv, t) }

  let conclude variable typeScheme (inferer: TypeInferer) =
    { inferer
      with
        TypeEnvironment =
          inferer.TypeEnvironment.Add(variable.Name, typeScheme)
        VariableTypeMap =
          inferer.VariableTypeMap |> Map.add variable.Id typeScheme
    }

  let local f (inferer: TypeInferer) =
    { f inferer with TypeEnvironment = inferer.TypeEnvironment }

  let unify t t' (inferer: TypeInferer) =
    { inferer with Substitution = inferer.Substitution |> Substitution.unify t t' }

  /// 式 expression の型を t と仮定する。
  /// t には型変数が含まれていることがあるため、その具体的な型はまだ分からないが、
  /// これを式 expression の構造にもとづいて決定する。
  /// 結果として、型 t を具体的な型に置換できるように、置換が拡張される。
  /// また、expresssion が変数を定義する場合は、環境が拡張される。
  /// expression の型が t になりえない場合はエラーとする。
  /// なお previous は直前に推論した式を表す。(エラー出力用に使う。)
  type private InferFunction(previous, expression, t, inferer) =
    let infer expression' t' inferer' =
      InferFunction(Some expression, expression', t', inferer').Run()

    member this.InferOperator(operator) =
      match operator with
      | AddOperator
      | MulOperator ->
        let tFun = FunTypeExpression
        let tInt = TypeExpression.int
        inferer |> unify t (tFun (tInt, tFun (tInt, tInt)))

    member this.InferVar(identifier) =
      match inferer.TypeEnvironment.TryFind(identifier) with
      | Some (typeScheme: TypeScheme) ->
        let t' = typeScheme.Instantiate()
        inferer |> unify t t'
      | None ->
        failwith "TODO: variable not defined"

    member this.InferFun(pattern, body) =
      match pattern with
      | UnitPattern _ ->
        let tu = TypeVariable.fresh () |> VarTypeExpression
        let t' = FunTypeExpression (TypeExpression.unit, tu)
        inferer
        |> unify t t'
        |> infer body tu
      | VariablePattern (_, variable) ->
        let tv = TypeVariable.fresh () |> VarTypeExpression
        let tu = TypeVariable.fresh () |> VarTypeExpression
        let t' = FunTypeExpression (tv, tu)
        inferer
        |> unify t t'
        |> local
            (fun inferer ->
              inferer
              |> conclude variable (TypeScheme ([||], tv))
              |> infer body tu
            )

    member this.InferIf(clauses) =
      let (inferer, elseExists) =
        clauses |> Array.fold
          (fun (inferer, elseExists) clause ->
            match clause with
            | IfClause (condition, expression) ->
              let inferer =
                inferer
                |> infer condition TypeExpression.bool
                |> infer expression t
              (inferer, elseExists)
            | ElseClause expression ->
              (inferer |> infer expression t, true)
          ) (inferer, false)
      if elseExists
      then inferer
      else inferer |> unify t TypeExpression.unit

    member this.InferApply(left, right) =
      let tv = TypeVariable.fresh () |> VarTypeExpression
      inferer
      |> infer left (FunTypeExpression (tv, t))
      |> infer right tv

    member this.InferThen(left, right) =
      inferer
      |> infer left TypeExpression.unit
      |> infer right t

    member this.InferVal(pattern, right) =
      match pattern with
      | UnitPattern _ ->
        inferer
        |> unify t TypeExpression.unit
        |> infer right TypeExpression.unit
      | VariablePattern (_, variable) ->
        let tv = TypeVariable.fresh () |> VarTypeExpression
        inferer
        |> unify t TypeExpression.unit
        |> infer right tv
        |>  (fun inferer ->
              let t = inferer.TypeEnvironment.Generalize(inferer.Substitution.Apply(tv))
              inferer |> conclude variable t
            )

    member this.Run() =
      match expression with
      | UnitExpression _ ->
        inferer |> unify t TypeExpression.unit
      | IntExpression _ ->
        inferer |> unify t TypeExpression.int
      | BoolExpression _ ->
        inferer |> unify t TypeExpression.bool
      | OperatorExpression (_, operator) ->
        this.InferOperator(operator)
      | VarExpression (_, identifier) ->
        this.InferVar(identifier)
      | FunExpression (_, pattern, expression) ->
        this.InferFun(pattern, expression)
      | IfExpression (headClause, tailClauses) ->
        this.InferIf(Array.append [|headClause|] tailClauses)
      | ApplyExpression (_, left, right) ->
        this.InferApply(left, right)
      | ThenExpression (_, left, right) ->
        this.InferThen(left, right)
      | ValExpression (pattern, expression) ->
        this.InferVal(pattern, expression)

  let infer expression t inferer =
    InferFunction(None, expression, t, inferer).Run()
