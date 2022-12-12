// 注意:
//
// - ナイーブな実装
// - パーサーやUIはない
// - 初心者向け (ただし多相ラムダ計算は知っていること)
// - 実用的でない

module rec Program

open System

// 構文:
// (System F にリスト、bool、ペアを加えたもの)

/// 変数名と型変数名に使う
type Name = string

/// 項 (terms)
type Tm =
  | Var of Name
  /// `e : t`
  | Ann of Tm * Tp
  /// `λx:t. E` (型注釈は省略可)
  | Lam of Name * Tp option * Tm
  /// `e1 e2`
  | App of Tm * Tm
  /// `ΛT. e`
  | Tlam of Name * Tm
  /// `e [t]` (型のインスタンス化)
  | Inst of Tm * Tp
  /// `let x : t = e in e'` (型注釈は省略可)
  | Let of Name * Tp option * Tm * Tm
  /// `(e1, e2)`
  | Pair of Tm * Tm
  /// `e.1`
  | Fst of Tm
  /// `e.2`
  | Snd of Tm
  | TrueE
  | FalseE
  /// `if e then e1 else e2`
  | ITE of Tm * Tm * Tm
  /// `{e1, e2, ...}` (リストリテラル)
  | List of Tm list

/// 型 (types)
[<CustomEquality; NoComparison>]
type Tp =
  /// 型変数
  | Tvar of Name
  /// `t1 → t2`
  | Arrow of Tp * Tp
  /// `t1 * t2`
  | Prod of Tp * Tp
  /// `∀a. t`
  | Forall of Name * Tp
  | BoolT
  | ListT of Tp

  // F# Tips: Tpの比較関数をオーバーライドする (Equals, GetHashCode, IEquatable<_>.Equals)
  override this.Equals(other: obj) =
    match other with
    | :? Tp as other -> tpEquals this other
    | _ -> false

  override _.GetHashCode() = failwith "not used"

  interface IEquatable<Tp> with
    member this.Equals(other) = tpEquals this other



// 目標はユーザープログラムである項を受け取って、それの有効な型を返すこと、あるいは拒否すること
// (ただしSystem Fの型推論は決定不能(undecidable)なので型注釈を要求する場合がある)
// 型注釈が適切である (その型が対象の項の有効な型である) ことを検査する
// 加えて、ユーザーによって与えられた型が well-formed である (閉じている) ことも検査する

// アルゴリズムは主に2つの相互再帰関数によって定義される
//
//    infer : Ctx -> Tm -> Tp
//        項の型を推論して返す。あるいはエラーを起こす
//
//    check : Ctx -> Tm -> Tp -> unit
//        項が指定された型を持つことを確かめる。あるいはエラーを起こす
//
// ここで `Ctx` (context) は値と型の環境を表す
// (スコープ内に定義されている変数の名前と型、および型変数の名前を持つ)

type Ctx =
  { Vars: (Name * Tp) list
    Typs: Name list }

/// 空の環境
let emptyCtx: Ctx = { Vars = []; Typs = [] }

/// 環境に変数を加える
let assume (ctx: Ctx) nm tp =
  { ctx with Vars = (nm, tp) :: ctx.Vars }

/// 環境に型変数を加える
let assumeT (ctx: Ctx) nm = { ctx with Typs = nm :: ctx.Typs }

/// 変数の型を取り出す
let lookupT (ctx: Ctx) nm =
  ctx.Vars
  |> List.tryPick (fun (n, tp) -> if n = nm then Some tp else None)

/// 指定した名前の型変数が定義されているかどうか
let isTvar (ctx: Ctx) nm = ctx.Typs |> List.contains nm



// 型検査のアルゴリズムの前に重要な関数を2つ定義する (subs, tyEquals)

// 型代入 (substitution)
//
//    開いた型 T に含まれる自由変数 a を型 S に置き換える操作
//    (`T[S/a]` や `T[a ↦ S]` と書く)
//    (自由変数は `Λ` に量化されていない型変数のこと。開いた型は自由変数を持つ型のこと)
//
//    例: `(a → int)[bool/a]` = `bool → int`
//
//    変数のシャドウイングに注意
//    同じ名前の異なる変数が出現していることがある
//    例: (a → (∀a. a → int))[bool/a]
//        = bool → (∀a. a → int)
//    内側で `Λ` に束縛されている `a` は、外側の `a` とは異なる変数である
//    外側の `a` への代入は内側の `a` に影響しない
//    (α同値性を考えると明らか)

let subs (typ: Tp) (nm: Name) (newTp: Tp) =
  match typ with
  | Tvar nm' -> if nm' = nm then newTp else typ

  | Arrow (ltyp, rtyp) -> Arrow(subs ltyp nm newTp, subs rtyp nm newTp)

  | Prod (ltyp, rtyp) -> Prod(subs ltyp nm newTp, subs rtyp nm newTp)

  | Forall (nm', body) ->
    if nm = nm' then
      typ
    else
      Forall(nm', subs body nm newTp)

  | BoolT -> BoolT
  | ListT t -> ListT(subs t nm newTp)



// 型の同値性
//
//    型の同値性は想定している等式理論によっていろいろ考えられるが、
//    ここでは単にα同値性を検査する
//    (すなわち、型変数の名前を付け替えることで同一になる型同士は等しいとみなす)
//    例: ∀a. a → a
//      = ∀b. b → b
//
//    型の同値性を決定する手続きはよく conversion checking と呼ばれ、
//    2つの型が等しいときそれらの型は互いに convertible であるという

let private tpEquals (l: Tp) (r: Tp) =
  match l, r with
  | (Tvar nm1, Tvar nm2) -> nm1 = nm2

  | (Arrow (ltyp1, rtyp1), Arrow (ltyp2, rtyp2)) -> tpEquals ltyp1 ltyp2 && tpEquals rtyp1 rtyp2
  | (Prod (ltyp1, rtyp1), Prod (ltyp2, rtyp2)) -> tpEquals ltyp1 ltyp2 && tpEquals rtyp1 rtyp2
  | (BoolT, BoolT) -> true
  | (ListT typ1, ListT typ2) -> tpEquals typ1 typ2

  | (Forall (nm1, body1), Forall (nm2, body2)) ->
    // α同値性のため、両者が量化する型変数を同名にしてから比較する
    tpEquals (subs body2 nm2 (Tvar nm1)) body1

  | _ -> false

/// 型が一致することを確かめる。一致しなければ例外を投げる
let conv typ1 typ2 =
  if not (tpEquals typ1 typ2) then
    failwithf "型が一致しません:\n  %A\n  <> %A" typ1 typ2



// 型検査の主要な関数を定義する
//
// 「双方向型検査のレシピ」(the bidi recipe) という、
// inferとcheckのどちらを使うべきかのおおまかな指針がある:
//
//    - 除去規則は infer すること (適用、タプルの射影、if/then/else)
//    - 導入規則は check すること (ラムダ式、ペアの構築)
//
// 双方向型検査によって型が式に「流れ込む」ので、実際に型注釈が要求されるのは「最上位」であることが多い
// 上記のレシピに従っておくと型注釈はいわゆる redex (簡約可能式) だけになるので、このレシピは有用である
//
//    - (λx.e1) e2` (ラムダに直接適用する)
//    - `(e1, e2).1` (ペアから直接射影する)
//
// 実際のコードにこういう簡約可能な式はあまり出現しないので、(項の)最上位を除いて、型注釈が必要になることは少ない

// 型検査の基本的な前提・不変条件は以下の通り
//
// - 環境(Ctx)に含まれる型はすべてwell-formedであること
// - `infer` が返す型は必ずwell-formedであること
// - `check` に渡される型はwell-formedであること
// - ユーザーから与えられた型は検査すること

let infer (ctx: Ctx) (tm: Tm) : Tp =
  match tm with
  // 変数の型は単に環境から取り出す
  | Var nm ->
    match lookupT ctx nm with
    | Some typ -> typ
    | None -> failwithf "変数が定義されていません (%s)" nm

  // ラムダの引数に型注釈がある場合、本体を推論する
  | Lam (nm, Some intyp, body) ->
    wellFormed ctx intyp
    let outtyp = infer (assume ctx nm intyp) body
    Arrow(intyp, outtyp)

  // 適用は、左辺が推論可能でその型が関数型だった場合に推論できる
  | App (tm1, tm2) ->
    let tp1 = infer ctx tm1

    match tp1 with
    | Arrow (ltyp1, rtyp1) ->
      check ctx tm2 ltyp1
      rtyp1
    | _ -> failwithf "型エラー: この項は関数でないため、関数適用できません: %A" tm1

  // 型ラムダと型適用 (インスタンス化) は項のもの (ラムダ・適用) と同様
  | Tlam (nm, body) ->
    let bodytyp = infer (assumeT ctx nm) body
    Forall(nm, bodytyp)

  | Inst (tm, typ) ->
    wellFormed ctx typ
    let tmtyp = infer ctx tm

    match tmtyp with
    | Forall (nm, body) -> subs body nm typ
    | _ -> failwithf "型エラー: 全称型でない項をインスタンス化できません: %A" tm

  // `checkLet` を参照
  | Let (nm, typ, body, rest) ->
    let localCtx = checkLet ctx nm typ body
    infer localCtx rest

  // ペアは可能なら推論する
  | Pair (tm1, tm2) -> Prod(infer ctx tm1, infer ctx tm2)

  // 射影は除去規則なので推論可能なはず
  | Fst tm ->
    match infer ctx tm with
    | Prod (typ1, _) -> typ1
    | _ -> failwithf "型エラー: ペアでない型の射影はできません: %A" tm

  | Snd tm ->
    match infer ctx tm with
    | Prod (_, typ2) -> typ2
    | _ -> failwithf "型エラー: ペアでない型の射影はできません: %A" tm

  | TrueE
  | FalseE -> BoolT

  // 型注釈を持つ項を推論するとき、その型は指定通りの型とみなせるが、
  // 指定された型が注釈対象の項に対して有効であることを検査する必要がある
  // これは infer から check への「方向転換」といえる
  | Ann (tm, typ) ->
    // (訳注 wellFormed typ しなくていいのだろうか)
    check ctx tm typ
    typ

  | Lam _
  | ITE _
  | List _ -> failwithf "型エラー: この項の型を推論できません: %A\n\n  (ヒント: 型注釈が必要かもしれません)" tm



let check (ctx: Ctx) (tm: Tm) (typ: Tp) : unit =
  match tm, typ with
  // 引数に注釈のないラムダ式は、infer は難しいが check はできる
  | Lam (nm, None, body), Arrow (ltyp, rtyp) ->
    // 引数の型を関数の入力の型と仮定して、本体が出力の型であることを検査する
    check (assume ctx nm ltyp) body rtyp

  | Tlam (nm1, body), Forall (nm2, typ) ->
    let subsTyp = subs typ nm2 (Tvar nm1)
    check (assumeT ctx nm1) body subsTyp

  // `checkLet` を参照
  | Let (nm, typ, body, rest), bodytyp ->
    let localCtx = checkLet ctx nm typ body
    check localCtx rest bodytyp

  | Pair (tm1, tm2), Prod (typ1, typ2) ->
    check ctx tm1 typ1
    check ctx tm2 typ2

  // if式はinferできるが、ここではcheckも実装する
  // (片方のブランチのみ推論可能な場合はどうなるか、考えてみよう)
  | ITE (cond, tcase, fcase), _ ->
    check ctx cond BoolT
    check ctx tcase typ
    check ctx fcase typ

  // リストもif式と同様、infer可能ではあるが実装する
  // もし1つの要素が推論可能だったら、あるいはリストが空だったらどうなるか、考えてみよう
  | List tms, ListT typ ->
    for tm in tms do
      check ctx tm typ

  // その他の場合: 項を推論できるなら推論して、その型が期待される型と等しいか確認する
  // (checkできる項はinferできる項の上位集合ということになる)
  | _, expected ->
    let actual = infer ctx tm
    conv expected actual



// let束縛のための再利用可能な関数
// 考えかたとしては、`let x = e in e'` は `e` が推論可能なときだけinferができる
// また、`e'` がcheck可能なときだけcheckできる
// この関数によって主な処理(main logic)を抽象化する
// 拡張済みの文脈を返す
let checkLet (ctx: Ctx) (nm: Name) (typOpt: Tp option) (body: Tm) : Ctx =
  match typOpt with
  | None ->
    let bodytyp = infer ctx body
    assume ctx nm bodytyp

  | Some typ ->
    wellFormed ctx typ
    check ctx body typ
    assume ctx nm typ



/// 型がwell-formedであることを確かめて、そうでなければ例外を投げる
/// (型変数がすべて束縛されていること)
let wellFormed ctx typ : unit =
  match typ with
  | Tvar nm ->
    if not (isTvar ctx nm) then
      failwithf "型変数が定義されていません (%s)" nm

  | Arrow (ltyp, rtyp)
  | Prod (ltyp, rtyp) ->
    wellFormed ctx ltyp
    wellFormed ctx rtyp
  | Forall (nm, body) -> wellFormed (assumeT ctx nm) body
  | BoolT -> ()
  | ListT t -> wellFormed ctx t


//　アルゴリズムは以上。以下はサンプルの実行:

[<EntryPoint>]
let main _ =
  let testCheck (tmLabel, tm) (tpLabel, tp) =
    printfn "check %s against %s" tmLabel tpLabel
    check emptyCtx tm tp

  let testInfer (tmLabel, tm) =
    printf "infer %s ~>" tmLabel
    stdout.Flush()
    let typ = infer emptyCtx tm
    printfn " %A" typ

  // id関数のcheck
  testCheck
    ("ΛA. λx:A. x", Tlam("A", (Lam("x", Some(Tvar "A"), Var "x"))))
    ("A → A", Forall("A", Arrow(Tvar "A", Tvar "A")))

  // idのcheck (内側のラムダに型注釈がないバージョン)
  testCheck ("ΛA. λx. x", Tlam("A", Lam("x", None, Var "x"))) ("A → A", Forall("A", Arrow(Tvar "A", Tvar "A")))

  // idのinfer (内側のラムダに型注釈があるバージョン)
  testInfer ("ΛA. λx:A. x", Tlam("A", Lam("x", Some(Tvar "A"), Var "x")))

  // 高階のパラメータ (多相関数であるidを引数に取る関数)
  testInfer (
    "λid:(∀A. A → A). (id [bool] true, id [list bool] {})",
    Lam(
      "id",
      Some(Forall("A", Arrow(Tvar "A", Tvar "A"))),
      (Pair(App(Inst(Var "id", BoolT), TrueE), App(Inst(Var "id", ListT BoolT), (List []))))
    )
  )

  // 条件分岐
  testInfer (
    """let boolAnd : bool → bool → bool =
  λb1. λb2. if b1 then b2 else false
in boolAnd true false
  """,
    Let(
      "boolAnd",
      Some(Arrow(BoolT, Arrow(BoolT, BoolT))),
      Lam("b1", None, Lam("b2", None, ITE(Var "b1", Var "b2", FalseE))),
      App(App(Var "boolAnd", TrueE), FalseE)
    )
  )

  printfn "OK"
  0
