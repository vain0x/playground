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

let aBinToPrim bin =
  match bin with
  | AEqBin ->
    KEqPrim, ByRef, ByRef

  | AAddBin ->
    KAddPrim, ByMove, ByMove

  | AAssignBin ->
    KAssignPrim, ByRef, ByMove

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

  | ACallTerm (Some (ANameTerm (AName (Some funName, _))), args, _) ->
    // 関数から戻ってきた後の計算を中間関数 ret と定める。
    // ret を追加の引数として渡して、関数にジャンプする。

    let res = context.FreshName (sprintf "%s_res" funName)
    let ret = context.FreshName (sprintf "%s_ret" funName)

    let rec go exit args =
      match args with
      | [] ->
        exit []

      | AArg (callBy, Some arg, _) :: args ->
        arg |> kgTerm context (fun arg ->
          args |> go (fun args ->
            KArg (callBy, arg) :: args |> exit
          ))

      | _ :: args ->
        go exit args

    args |> go (fun args ->
      let args = KArg (ByMove, KName ret) :: args
      KFix (
        ret,
        [KParam (ByMove, res)],
        exit (KName res),
        KApp (funName, args)
      ))

  | ABinTerm (Some bin, Some first, Some second, _) ->
    let prim, firstCallBy, secondCallBy = aBinToPrim bin
    let name = prim |> kPrimToString
    let res = context.FreshName (sprintf "%s_res" name)

    first |> kgTerm context (fun first ->
      second |> kgTerm context (fun second ->
        let args =
          [
            KArg (firstCallBy, first)
            KArg (secondCallBy, second)
          ]

        KPrim (prim, args, res, exit (KName res))
      ))

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

  | AExternFnStmt (Some (AName (Some funName, _)), args, _) ->
    // extern fn f(params);
    // ==> fn f(params) { extern_fn"f"(params) }
    // ==> fix f(ret, params) { let res = extern_fn"f"(params); ret(res) }

    let res = context.FreshName (sprintf "%s_res" funName)
    let ret = context.FreshName (sprintf "%s_ret" funName)

    let paramList =
      args |> List.choose (fun arg ->
        match arg with
        | AArg (callBy, Some (ANameTerm (AName (Some name, _))), _) ->
          KParam (callBy, context.FreshName name) |> Some

        | _ ->
          None
      )

    let args =
      paramList |> List.map (fun (KParam (callBy, name)) ->
        KArg (callBy, KName name)
      )

    KFix (
      funName,
      (KParam (ByMove, ret)) :: paramList,
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
