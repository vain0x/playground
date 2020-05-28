module rec RaiiLang.KirClosureConversion

open RaiiLang.Helpers
open RaiiLang.Kir

type Ident = string

/// 識別子の定義・使用の状況を表す。
[<Struct>]
type KnownContext =
  {
    /// 定義済みの識別子
    DefSet: Set<Ident>

    /// 使用されている識別子
    UseSet: Set<Ident>
  }

type KccContext =
  {
    /// 現在位置における識別子の定義・使用の状態
    mutable Current: KnownContext

    /// グローバル関数や定数など
    KnownSet: HashSet<Ident>

    /// 関数内の定義・使用文脈
    CaptureMap: HashMap<Ident, KnownContext>

    Funs: ResizeArray<KNode -> KNode>
  }

// -----------------------------------------------
// KnownContext
// -----------------------------------------------

let knownContextEmpty (): KnownContext =
  {
    DefSet = Set.empty
    UseSet = Set.empty
  }

let knownContextAddDef ident context =
  { context with
      DefSet = context.DefSet |> Set.add ident
  }

let knownContextAddUse ident context =
  { context with
      UseSet = context.UseSet |> Set.add ident
  }

let knownContextToCaptureMap (knownSet: HashSet<_>) (context: KnownContext) =
  Set.difference context.UseSet context.DefSet
  |> Set.filter (knownSet.Contains >> not)

// -----------------------------------------------
// KccContext
// -----------------------------------------------

let kccContextNew (): KccContext =
  {
    Current = knownContextEmpty ()
    KnownSet = HashSet()
    CaptureMap = HashMap()
    Funs = ResizeArray()
  }

let kccContextAddDef ident (context: KccContext) =
  context.Current <- context.Current |> knownContextAddDef ident

let kccContextAddUse ident (context: KccContext) =
  context.Current <- context.Current |> knownContextAddUse ident

let kccContextAddKnown ident (context: KccContext) =
  context.KnownSet.Add(ident) |> ignore

let kccContextSave funName (context: KccContext) =
  context.CaptureMap.Add(funName, context.Current)

let kccContextGetCaptureMap funName (context: KccContext) =
  let knownContext =
    match context.CaptureMap.TryGetValue(funName) with
    | true, knownContext ->
      knownContext

    | false, _ ->
      knownContextEmpty ()

  knownContext |> knownContextToCaptureMap context.KnownSet

/// 使用関係の推移閉包を取る。
let kccContextMakeFixPoint (context: KccContext) =
  let mutable stuck = false
  let mutable updateMap = ResizeArray()

  while not stuck do
    stuck <- true
    updateMap.Clear()

    for KeyValue (funName, knownContext) in context.CaptureMap do
      let mutable knownContext = knownContext
      let mutable modified = false

      for ident in knownContext.UseSet do
        let useSet = context |> kccContextGetCaptureMap ident
        if Set.isSubset useSet knownContext.UseSet |> not then
          knownContext <-
            { knownContext with
                UseSet = knownContext.UseSet |> Set.union useSet
            }
          modified <- true

      if modified then
        stuck <- false
        updateMap.Add(funName, knownContext)

    for funName, knownContext in updateMap do
      context.CaptureMap.[funName] <- knownContext

// -----------------------------------------------
// Analyze
// -----------------------------------------------

let kccAnalyzeArg context (KArg (callBy, arg)) =
  arg |> kccAnalyzeNode context

let kccAnalyzeNode context node =
  match node with
  | KInt _ ->
    ()

  | KName name ->
    context |> kccContextAddUse name

  | KPrim (_, args, result, next) ->
    for arg in args do
      arg |> kccAnalyzeNode context

    context |> kccContextAddDef result
    next |> kccAnalyzeNode context

  | KApp (cal, args) ->
    context |> kccContextAddUse cal

    for arg in args do
      arg |> kccAnalyzeArg context

  | KFix (funName, paramList, body, next) ->
    // 関数のスコープに入って定義・使用を検査する。
    let current = context.Current
    context.Current <-
      { context.Current with
          DefSet = Set.empty
          UseSet = Set.empty
      }

    for KParam (_, param) in paramList do
      context |> kccContextAddDef param

    body |> kccAnalyzeNode context

    context |> kccContextSave funName

    // 関数のスコープから戻る。
    context.Current <- current

    // 関数はクロージャ変換によってローカル変数を参照しなくなるので、
    // グローバル関数とみなしてよい。
    context |> kccContextAddKnown funName

    next |> kccAnalyzeNode context

// -----------------------------------------------
// Transform
// -----------------------------------------------

let kccTransformArg context (KArg (callBy, arg)) =
  arg |> kccTransformNode context

let kccTransformNode context node =
  match node with
  | KInt _
  | KName _ ->
    node

  | KPrim (prim, args, result, next) ->
    let next = next |> kccTransformNode context
    KPrim (prim, args, result, next)

  | KApp (cal, args) ->
    let captureMap =
      context |> kccContextGetCaptureMap cal

    // 関数がキャプチャする変数を追加の引数として渡す。
    let args =
      args @ (
        captureMap
        |> Set.toList
        |> List.map (fun name -> KArg (ByRef, KName name))
      )

    KApp (cal, args)

  | KFix (funName, paramList, body, next) ->
    let captureMap =
      context |> kccContextGetCaptureMap funName

    // 関数がキャプチャする変数を追加の引数として受け取る。
    let paramList =
      paramList @ (
        captureMap
        |> Set.toList
        |> List.map (fun name -> KParam (ByRef, name))
      )

    let body = body |> kccTransformNode context

    // 関数はトップレベルに巻き上げる。
    context.Funs.Add(fun next -> KFix (funName, paramList, body, next))

    next |> kccTransformNode context

let kirClosureConversion (node: KNode) =
  let context = kccContextNew ()

  node |> kccAnalyzeNode context
  context |> kccContextMakeFixPoint

  let node = node |> kccTransformNode context
  let node =
    // 関数をトップレベルに移動する。
    let funs = context.Funs
    funs.Reverse()
    funs |> Seq.fold (fun next makeFun -> makeFun next) node

  node
