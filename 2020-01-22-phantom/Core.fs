module rec Core

type Pat =
  | IntPat
    of int

  | RangePat
    of Pat option * Pat option

  | TyPat
    of Ty

type Constr =
  /// self @ pat
  | CaseConstr
    of KTerm * Pat

type Ensure =
  Constr * Constr

type Phase =
  {
    PhaseName: string
    Constrs: Constr list
  }

type Phantom =
  {
    PhantomName: string
    Phases: Phase list
  }

type Ty =
  | I64Ty

  | PhantomTy
    of Phantom

  | PhaseTy
    of Phase

type Local =
  {
    LocalName: string
    Ty: Ty
  }

type Fn =
  {
    FnName: string
    Params: Local list
    Result: Local
    ResultTy: Ty
    Ensures: Ensure list
    Body: KNode
  }

type KTerm =
  | KInt
    of int

  | KName
    of Local

  | KSelf

type KNode =
  | KReturn
    of KTerm

  | KJump
    of Fn * KTerm list

  | KCase
    of KTerm * Pat * KNode * KNode

  | KCall
    of Fn * KTerm list * result:Local * KNode

type Space =
  | IntRangeSpace
    of int option * int option

  | UnionSpace
    of Space list

  | TySpace
    of Ty

// -----------------------------------------------
// 型システム
// -----------------------------------------------

/// first <: second ?
let tyIsSubtypeOf first second =
  match first, second with
  | I64Ty, I64Ty ->
    true

  | PhantomTy first, PhantomTy second
    when first.PhantomName = second.PhantomName ->
    true

  | PhaseTy first, PhaseTy second
    when first.PhaseName = second.PhaseName ->
    true

  | PhaseTy first, PhantomTy second
    when second.Phases |> List.exists (fun phase -> phase.PhaseName = first.PhaseName) ->
    true

  | _ ->
    false

// -----------------------------------------------
// Space
// -----------------------------------------------

let spaceEmpty =
  UnionSpace []

let spaceUnion spaces =
  let rec go spaces acc =
    match spaces with
    | [] ->
      List.rev acc

    | UnionSpace subspaces :: spaces ->
      acc |> go subspaces |> go spaces

    | space :: spaces when space |> spaceIsEmpty ->
      acc |> go spaces

    | space :: spaces ->
      (space :: acc) |> go spaces

  match go spaces [] with
  | [space] ->
    space

  | spaces ->
    UnionSpace spaces

let rec spaceIsEmpty space =
  let result =
    match space with
    | IntRangeSpace (first, second) ->
      match first, second with
      | Some first, Some second ->
        first >= second
        
      | _ ->
        false

    | UnionSpace spaces ->
      spaces |> List.forall spaceIsEmpty

    | TySpace _ ->
      false

  result

let spaceTryDecompose space =
  match space with
  | TySpace I64Ty ->
    IntRangeSpace (None, None) |> Some

  | TySpace (PhantomTy phantom) ->
    phantom.Phases |> List.map (PhaseTy >> TySpace) |> UnionSpace |> Some

  | TySpace (PhaseTy phase) ->
    match phase.Constrs with
    | [CaseConstr (KSelf, pat)] ->
      patToSpace pat |> Some

    | _ ->
      None

  | IntRangeSpace _
  | UnionSpace _ ->
    None

let spaceFromInt value =
  let upperbound =
    if value = System.Int32.MaxValue then
      None
    else
      Some (value + 1)

  IntRangeSpace (Some value, upperbound)

let patToSpace pat =
  match pat with
  | IntPat value ->
    spaceFromInt value

  | RangePat (first, second) ->
    let l =
      match first with
      | Some (IntPat value) ->
        Some value
      | _ ->
        None

    let r =
      match second with
      | Some (IntPat value) ->
        Some value
      | _ ->
        None

    IntRangeSpace (l, r)

  | TyPat ty ->
    TySpace ty

/// スペースの例となるパターンを1つ構築する。
let spaceToAnyPat space =
  match space with
  | IntRangeSpace (l, r) ->
    match l, r with
    | Some l, _ ->
      IntPat l |> Some

    | None, Some r ->
      IntPat (r - 1) |> Some

    | None, None ->
      IntPat 0 |> Some

  | UnionSpace spaces ->
    spaces |> List.tryPick spaceToAnyPat

  | TySpace ty ->
    TyPat ty |> Some

// -----------------------------------------------
// ヘルパー
// -----------------------------------------------

let localOf name ty =
  {
    LocalName = name
    Ty = ty
  }

let phaseOfCase name pat =
  {
    PhaseName = name
    Constrs = [
      CaseConstr pat
    ]
  }
