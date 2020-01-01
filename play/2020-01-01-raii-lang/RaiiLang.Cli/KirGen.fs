module rec RaiiLang.KirGen

open RaiiLang.Helpers
open RaiiLang.Kir
open RaiiLang.Syntax

let unitNode = KInt "0"

let binToPrim bin =
  match bin with
  | AEqBin ->
    KEqPrim

  | AAddBin ->
    KAddPrim

  | AAssignBin ->
    KAssignPrim

let kgTerm exit term =
  match term with
  | AIntLiteral (Some intText, _) ->
    KInt intText |> exit

  | ANameTerm (AName (Some name, _)) ->
    KName name |> exit

  | AGroupTerm (Some term, _) ->
    kgTerm exit term

  | ABlockTerm (stmts, _) ->
    kgStmts exit stmts

  | ACallTerm (Some (ANameTerm (AName (Some funName, _))), [AArg (callBy, Some arg, _)], _) ->
    // 各引数を評価する。
    // 関数から戻ってきた後の計算を中間関数 ret と定める。
    // ret を追加の引数として渡して、関数にジャンプする。
    let ret = freshName (sprintf "%s_ret" funName)
    let res = freshName (sprintf "%s_res" funName)

    arg |> kgTerm (fun arg ->
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
    let res = freshName (string prim)

    first |> kgTerm (fun first ->
      second |> kgTerm (fun second ->
        KPrim (
          prim,
          [first; second],
          res,
          exit (KName res)
        )))

  | _ ->
    failwithf "unimpl %A" term

let kgStmt exit stmt =
  match stmt with
  | ATermStmt (Some term, _) ->
    kgTerm exit term

  | ALetStmt (Some (ANameTerm (AName (Some varName, _))), Some body, _) ->
    // 右辺を計算する。
    // 後続の計算を行う中間関数 next を定義する。
    // 計算結果を引数に渡して next にジャンプする。

    let funName = freshName (sprintf "%s_next" varName)
    let callBy = ByMove // call-by を指定する構文がまだない

    body |> kgTerm (fun body ->
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

    let ret = freshName (sprintf "%s_ret" funName)

    let args =
      args |> List.choose (fun arg ->
        match arg with
        | AArg (callBy, Some (ANameTerm (AName (Some name, _))), _) ->
          KParam (callBy, freshName name) |> Some
        | _ ->
          None
      )

    KFix (
      funName,
      (KParam (ByMove, ret)) :: args,
      kgTerm
        (fun res -> KApp (ret, [KArg (ByMove, res)]))
        body,
      exit unitNode
    )

  | ASemiStmt (stmts, _) ->
    kgStmts exit stmts

  | _ ->
    failwithf "unimpl %A" stmt

let kgStmts exit stmts =
  match stmts with
  | [] ->
    exit unitNode

  | [stmt] ->
    kgStmt exit stmt

  | stmt :: stmts ->
    stmt |> kgStmt (fun _ -> kgStmts exit stmts)

let kirGen (stmt: AStmt) =
  kgStmt id stmt
