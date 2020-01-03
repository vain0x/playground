module rec RaiiLang.KirGen

open RaiiLang.Helpers
open RaiiLang.Kir
open RaiiLang.Syntax

let unitNode = KInt "0"

[<Struct>]
type KirGenLoop =
  {
    BreakFun: string
    ContinueFun: string
  }

type KirGenContext =
  {
    FreshName: string -> string
    mutable LoopStack: KirGenLoop list
  }

let kgContextNew (): KirGenContext =
  {
    FreshName = freshNameFun ()
    LoopStack = []
  }

let kgTerm (context: KirGenContext) exit term =
  match term with
  | ABoolLiteral (value, _) ->
    KBool value |> exit

  | AIntLiteral (Some intText, _) ->
    KInt intText |> exit

  | AStrLiteral (segments, _) ->
    KStr segments |> exit

  | ANameTerm (AName (Some name, _)) ->
    KName name |> exit

  | AGroupTerm (Some term, _) ->
    kgTerm context exit term

  | ABlockTerm (stmts, _) ->
    kgStmts context exit stmts

  | ABreakTerm _ ->
    match context.LoopStack with
    | [] ->
      failwith "break out of loop"

    | { BreakFun = breakFun } :: _ ->
      KApp (breakFun, [])

  | AContinueTerm _ ->
    match context.LoopStack with
    | [] ->
      failwith "continue out of loop"

    | { ContinueFun = continueFun } :: _ ->
      KApp (continueFun, [])

  | ALoopTerm (Some body, _) ->
    // loop { body }; k
    // ==> fix break() { k }
    //     fix continue() { let _ = body; continue() }
    //     continue()

    let breakFun = context.FreshName "break"
    let continueFun = context.FreshName "continue"
    let continueNode = KApp (continueFun, [])

    let loopStack = context.LoopStack
    context.LoopStack <-
      {
        BreakFun = breakFun
        ContinueFun = continueFun
      } :: loopStack

    let onBreak = exit unitNode
    let onContinue = body |> kgTerm context (fun _ -> continueNode)

    context.LoopStack <- loopStack

    KFix (
      breakFun,
      [],
      onBreak,
      KFix (
        continueFun,
        [],
        onContinue,
        KApp (continueFun, [])
      ))

  | ACallTerm (Some (ANameTerm (AName (Some funName, _))), args, _) ->
    // 関数から戻ってきた後の計算を中間関数 ret と定める。
    // ret を追加の引数として渡して、関数にジャンプする。

    let res = context.FreshName (sprintf "%s_res" funName)
    let ret = context.FreshName (sprintf "%s_ret" funName)

    let rec go exit args =
      match args with
      | [] ->
        exit []

      | AArg (passBy, Some arg, _) :: args ->
        arg |> kgTerm context (fun arg ->
          args |> go (fun args ->
            KArg (passBy, arg) :: args |> exit
          ))

      | _ :: args ->
        go exit args

    args |> go (fun args ->
      let args = KArg (ByMove, KName ret) :: args
      KFix (
        ret,
        [KParam (MutMode, res)],
        exit (KName res),
        KApp (funName, args)
      ))

  | ABinTerm (Some bin, Some first, Some second, _) ->
    let prim = kPrimFromBin bin
    let name = prim |> kPrimToString
    let res = context.FreshName (sprintf "%s_res" name)

    first |> kgTerm context (fun first ->
      second |> kgTerm context (fun second ->
        let args =
          List.zip (kPrimToSig prim) [first; second]
          |> List.map KArg

        KPrim (prim, args, res, exit (KName res))
      ))

  | AIfTerm (Some cond, body, alt, _) ->
    // body または alt の結果を受け取って後続の計算を行う関数 if_next をおく。
    // 条件式の結果に基づいて body または alt を計算して、
    // その結果をもって if_next を呼ぶ。

    let funName = context.FreshName "if_next"
    let next res = KApp (funName, [KArg (ByMove, res)])

    let res = context.FreshName "res"

    cond |> kgTerm context (fun cond ->
      KFix (
        funName,
        [KParam (MutMode, res)],
        exit (KName res),
        KIf (
          cond,
          body
          |> Option.map (kgTerm context next)
          |> Option.defaultWith (fun () -> next unitNode),
          alt
          |> Option.map (kgTerm context next)
          |> Option.defaultWith (fun () -> next unitNode)
        )))

  | _ ->
    failwithf "unimpl %A" term

let kgStmt context exit stmt =
  match stmt with
  | ATermStmt (Some term, _) ->
    kgTerm context exit term

  | ALetStmt
      (
        Some (AParam (mode, Some (AName (Some varName, _)), _)),
        Some (AArg (passBy, Some body, _)),
        _
      ) ->
    // 右辺を計算する。
    // 後続の計算を行う中間関数 next を定義する。
    // 計算結果を引数に渡して next にジャンプする。

    let funName = context.FreshName (sprintf "%s_next" varName)

    body |> kgTerm context (fun body ->
      KFix (
        funName,
        [KParam (mode, varName)],
        (exit (KName varName)),
        KApp (funName, [KArg (passBy, body)])
    ))

  | AExternFnStmt (Some (AName (Some funName, _)), args, _) ->
    // extern fn f(params);
    // ==> fn f(params) { extern_fn"f"(params) }
    // ==> fix f(ret, params) { let res = extern_fn"f"(params); ret(res) }

    let res = context.FreshName (sprintf "%s_res" funName)
    let ret = context.FreshName (sprintf "%s_ret" funName)

    let paramList =
      args |> List.choose (fun arg ->
        match arg with
        | AParam (mode, Some (AName (Some name, _)), _) ->
          KParam (mode, context.FreshName name) |> Some

        | _ ->
          None
      )

    let args =
      paramList |> List.map (fun (KParam (mode, name)) ->
        KArg (mode |> modeToPassBy, KName name)
      )

    KFix (
      funName,
      (KParam (MutMode, ret)) :: paramList,
      KPrim (
        KExternFnPrim funName,
        args,
        res,
        KApp (
          ret,
          [KArg (ByMove, KName res)]
        )),
      exit unitNode
    )

  | AFnStmt (Some (AName (Some funName, _)), args, Some body, _) ->
    // 関数を fix で定義して、後続の計算を行う。

    // fn f(x) { return y; }; k
    // => fix f(ret, x) { ret(y); }; k

    let ret = context.FreshName (sprintf "%s_ret" funName)

    let args =
      args |> List.choose (fun arg ->
        match arg with
        | AParam (callBy, Some (AName (Some name, _)), _) ->
          KParam (callBy, context.FreshName name) |> Some

        | _ ->
          None
      )

    KFix (
      funName,
      (KParam (MutMode, ret)) :: args,
      kgTerm
        context
        (fun res -> KApp (ret, [KArg (ByMove, res)]))
        body,
      exit unitNode
    )

  | ASemiStmt (stmts, _) ->
    kgStmts context exit stmts

  | _ ->
    failwithf "unimpl %A" stmt

let kgStmts context exit stmts =
  match stmts with
  | [] ->
    exit unitNode

  | [stmt] ->
    kgStmt context exit stmt

  | stmt :: stmts ->
    stmt |> kgStmt context (fun _ -> kgStmts context exit stmts)

let kirGen (stmt: AStmt) =
  let context = kgContextNew ()
  kgStmt context id stmt
