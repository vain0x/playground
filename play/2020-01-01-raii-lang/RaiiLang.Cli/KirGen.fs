module rec RaiiLang.KirGen

open RaiiLang.Helpers
open RaiiLang.Kir
open RaiiLang.Syntax

let unitNode = KInt "0"

type KirGenContext =
  {
    FreshName: string -> string
  }

let kgContextNew (): KirGenContext =
  {
    FreshName = freshNameFun ()
  }

let binToPrim bin =
  match bin with
  | AEqBin ->
    KEqPrim

  | AAddBin ->
    KAddPrim

  | AAssignBin ->
    KAssignPrim

let kgTerm (context: KirGenContext) exit term =
  match term with
  | AIntLiteral (Some intText, _) ->
    KInt intText |> exit

  | ANameTerm (AName (Some name, _)) ->
    KName name |> exit

  | AGroupTerm (Some term, _) ->
    kgTerm context exit term

  | ABlockTerm (stmts, _) ->
    kgStmts context exit stmts

  | ACallTerm (Some (ANameTerm (AName (Some "assert", _))), [AArg (_, Some arg, _)], _) ->
    match arg with
    | ABinTerm (Some AEqBin, Some first, Some second, _) ->
      // assert(first == second) ==> assert_eq(first, second)
      let res = context.FreshName "assert_eq_res"
      first |> kgTerm context (fun first ->
        second |> kgTerm context (fun second ->
          KPrim (KAssertEqPrim, [first; second], res, exit (KName res))
        ))

    | _ ->
      // assert(cond) ==> assert_eq(cond, true)
      let res = context.FreshName "assert_res"
      arg |> kgTerm context (fun arg ->
        let trueTerm = KInt "1"
        KPrim (KAssertEqPrim, [arg; trueTerm], res, exit (KName res))
      )

  | ACallTerm (Some (ANameTerm (AName (Some funName, _))), [AArg (callBy, Some arg, _)], _) ->
    // 各引数を評価する。
    // 関数から戻ってきた後の計算を中間関数 ret と定める。
    // ret を追加の引数として渡して、関数にジャンプする。
    let ret = context.FreshName (sprintf "%s_ret" funName)
    let res = context.FreshName (sprintf "%s_res" funName)

    arg |> kgTerm context (fun arg ->
      KFix (
        ret, [KParam (callBy, res)], exit (KName res),
        KApp (
          funName,
          [
            KArg (ByMove, KName ret)
            KArg (callBy, arg)
          ])
      ))

  | ABinTerm (Some bin, Some first, Some second, _) ->
    // 演算結果を中間変数 res に束縛して、継続に中間変数を渡す。
    let prim = binToPrim bin
    let res = context.FreshName (string prim)

    first |> kgTerm context (fun first ->
      second |> kgTerm context (fun second ->
        KPrim (
          prim,
          [first; second],
          res,
          exit (KName res)
        )))

  | _ ->
    failwithf "unimpl %A" term

let kgStmt context exit stmt =
  match stmt with
  | ATermStmt (Some term, _) ->
    kgTerm context exit term

  | ALetStmt (Some (ANameTerm (AName (Some varName, _))), Some body, _) ->
    // 右辺を計算する。
    // 後続の計算を行う中間関数 next を定義する。
    // 計算結果を引数に渡して next にジャンプする。

    let funName = context.FreshName (sprintf "%s_next" varName)
    let callBy = ByMove // call-by を指定する構文がまだない

    body |> kgTerm context (fun body ->
      KFix (
        funName,
        [KParam (callBy, varName)],
        (exit (KName varName)),
        KApp (funName, [KArg (callBy, body)])
    ))

  | AFnStmt (Some (AName (Some funName, _)), args, Some body, _) ->
    // 関数を fix で定義して、後続の計算を行う。

    // fn f(x) { return y; }; k
    // => fix f(ret, x) { ret(y); }; k

    let ret = context.FreshName (sprintf "%s_ret" funName)

    let args =
      args |> List.choose (fun arg ->
        match arg with
        | AArg (callBy, Some (ANameTerm (AName (Some name, _))), _) ->
          KParam (callBy, context.FreshName name) |> Some
        | _ ->
          None
      )

    KFix (
      funName,
      (KParam (ByMove, ret)) :: args,
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
