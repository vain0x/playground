module rec Pcc.CodeGen

open System.Text
open Pcc.Ast
open Pcc.TypeCheck

let private sb = StringBuilder()

let private emitf fmt =
  Printf.kprintf (fun value -> sb.Append(value) |> ignore) fmt

/// 式を評価し、値がスタックに1つpushされるようなコードを生成する
let private transExpr ast nest env =
  match ast with
  | Expr.Num value -> emitf "  pushq $%d\n" value

  | Expr.Var v ->
    transVar v nest env

    // %rax に変数へのアドレスが入っているので、そこから値を読み出してプッシュする
    emitf "  movq (%%rax), %%rax\n"
    emitf "  pushq %%rax\n"

  | Expr.Str text -> failwith "Not Implemented"

  | Expr.Call ("new", _)
  | Expr.Call ("scan", _) -> failwith "Not Implemented"

  | Expr.Call ("+", [ left; right ]) ->
    transExpr left nest env
    transExpr right nest env

    // 右辺の値をポップする
    emitf "  popq %%rax\n"

    // スタックのトップにある値 (左辺の値) に、右辺の値を加算する
    emitf "  addq %%rax, (%%rsp)\n"

  | Expr.Call ("-", [ left; right ]) ->
    transExpr left nest env
    transExpr right nest env
    emitf "  popq %%rax\n"
    emitf "  subq %%rax, (%%rsp)\n"

  | Expr.Call ("*", [ left; right ]) ->
    transExpr left nest env
    transExpr right nest env
    emitf "  popq %%rax\n"
    emitf "  imulq %%rax, (%%rsp)\n"

  | Expr.Call ("/", [ left; right ]) ->
    transExpr left nest env
    transExpr right nest env

    emitf "  popq %%rax\n"
    emitf "  popq %%rbx\n"

    // %raxを符号拡張して、(%rdx, %rax) からなる16バイトの数値を作る
    // この16バイトの値を %rdx:%rax と表記する
    emitf "  cqto\n"

    // %rdx:%rax を%rbx (右辺の値) で割る。商が%rax、剰余が%rdxに入る
    // 剰余はいま使わない
    emitf "  idivq %%rbx\n"

    emitf "  pushq %%rax\n"

  | Expr.Call ("!", [ arg ]) ->
    transExpr arg nest env

    // スタックトップの値の符号を反転する
    emitf "  negq (%%rsp)\n"

  | Expr.Call (name, args) ->
    transStmt (Stmt.CallProc(name, args)) nest env

    // 呼び出し規約として、関数呼び出しの結果は%raxレジスタに格納される
    emitf "  pushq %%rax\n"

/// %raxレジスタに参照されるアドレスを入れる
let private transVar ast nest env =
  match ast with
  | Var name ->
    let vi: VarInfo =
      match lookup name env with
      | Some (Entry.Var vi) -> vi
      | _ -> failwithf "Undefined variable '%s'" name

    // 静的リンクをたどる。
    // いまのスタックフレームへのポインタ (%rbp) からはじめて、
    // (nest - level) 回だけ静的リンクをたどって、
    // その結果として%raxが対象のフレームを指すようにする
    emitf "  movq %%rbp, %%rax\n"

    for _ in 0 .. nest - vi.Level - 1 do
      // フレームの先頭から16バイト先に静的リンク (前のフレームのアドレス) が配置されているので、それを読み出す
      emitf "  movq -16(%%rax), %%rax\n"

    // フレーム内のオフセットだけアドレスをずらして、変数へのアドレスを得る
    //    %rax ← &(%rax)[offset]
    emitf "  leaq %d(%%rax), %%rax\n" vi.Offset

  | IndexedVar (name, index) ->
    // インデックスが表している、配列の先頭と要素の距離がスタックトップに置かれる
    //    push ((index) * 8)
    // 後で%rbxにポップする
    transExpr (Expr.Call("*", [ Expr.Num 8; index ])) nest env

    transVar (Var name) nest env

    // %raxに配列の先頭へのアドレスが入っているので、それを読み出す
    emitf "  movq (%%rax), %%rax\n"

    emitf "  popq %%rbx\n"

    //    %rax ← &(%rax)[%rbx]
    emitf "  leaq (%%rax, %%rbx), %%rax\n"

let private calcSize ty = 0

/// 文を評価するコードを生成する
let private transStmt ast nest env =
  typeStmt ast env

  match ast with
  | Stmt.Assign (v, e) ->
    // 右辺を評価する。その値がスタックのトップに入るので、後でポップする
    transExpr e nest env
    transVar v nest env

    // 左辺が指すアドレスが%raxに入っていて、そこに値をポップする。
    // ポップされる値は前述の通り右辺の値なので、これだけで代入になる
    emitf "  popq (%%rax)\n"

  | Stmt.CallProc ("new", [ Expr.Var v ]) ->
    // これをしたい:
    //    v = malloc(size);

    let size = calcSize (typeVar v env)
    emitf "  movq $%d, %%rdi\n" size
    emitf "  callq malloc\n"

    // mallocが返したアドレスが%raxに入っている。
    // 次の `transVar` の間に%raxが書き換わってしまうので、
    // 値をスタックに退避して、後でポップする
    emitf "  pushq %%rax\n"

    transVar v nest env

    // %raxに変数へのアドレスが入っている。
    // そこにmallocが返したアドレスを代入する
    emitf "  popq (%%rax)\n"

  | Stmt.CallProc ("scan", [ Expr.Var v ]) ->
    // これをしたい:
    //    scanf("%lld", &v);

    transVar v nest env

    // 書式文字列 "%lld" へのアドレスを第1引数に入れる
    emitf "  leaq IO(%%rip), %%rdi\n"

    // 書き込み先の変数のアドレスを第2引数に入れる
    emitf "  movq %%rax, %%rsi\n"

    emitf "  mov $0, %%rax\n"
    emitf "  callq scanf\n"

  | Stmt.CallProc ("sprint", [ Expr.Str value ]) ->
    // iprintと同様

    // 適当に一意な整数値。前にLをつけたものを文字列データのラベルに使う
    let id = sb.Length

    // 文字列を静的領域に配置する
    emitf "  .data\n"
    emitf "L%d:\n" id
    emitf "  .string \"%s\"\n" value

    emitf "  .text\n"
    emitf "  leaq L%d(%%rip), %%rdi\n" id
    emitf "  movq $0, %%rax\n"
    emitf "  callq printf\n"

  | Stmt.CallProc (name, args) ->
    for arg in args |> List.rev do
      transExpr arg nest env

    match name with
    | "iprint" ->
      // libcのprintf関数を呼びたい。引数の値を%rsi(第1引数として渡るレジスタ)へポップする
      emitf "  popq %%rsi\n"
      // IOラベル(%lldという文字列)のアドレスを%rdi(第2引数として渡るレジスタ)に入れる
      // 「ラベル(%rip)」は一種のイディオムで、ラベルへのアドレスを表す
      // (プログラムカウンタが入っているレジスタである%ripに対して、
      //  ラベルは現在のプログラムカウンタからの相対値なので、
      //  結果的にラベルのアドレスになる)
      emitf "  leaq IO(%%rip), %%rdi\n"

      // %raxを0にする。(なぜかは書かれていない。浮動小数点数の個数がゼロだから？)
      emitf "  movq $0, %%rax\n"

      emitf "  callq printf\n"

    | _ -> failwith "Not Implemented"

  | Stmt.Block (_, _) -> failwith "Not Implemented"
  | Stmt.If (_, _, _) -> failwith "Not Implemented"
  | Stmt.While (_, _) -> failwith "Not Implemented"

  | Stmt.Nil -> ()

let private transDec ast nest tEnv env = ()

let codeGen ast =
  sb.Clear() |> ignore

  // scan, iprint で使う書式文字列
  emitf "IO: .string \"%%lld\"\n"

  emitf "    .text\n"
  failwith "Not Implemented"
