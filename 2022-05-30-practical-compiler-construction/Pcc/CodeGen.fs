module rec Pcc.CodeGen

open System.Text
open Pcc.Ast
open Pcc.TypeCheck

let inline private todo () = failwith "Not Implemented"
let inline private unreachable () = failwith "unreachable"

/// 生成中の式や文を評価するコードを書き込むところ
let private sCode = ref (StringBuilder())

/// 関数の定義を書き込むところ
let private sOutput = StringBuilder()

let private sLabelRef = ref 0

let private freshLabel () =
  incr sLabelRef
  sLabelRef.contents

/// コードを書き込む
let private emitf fmt =
  Printf.kprintf (fun value -> sCode.contents.Append(value) |> ignore) fmt

/// 式を評価し、値がスタックに1つpushされるようなコードを生成する
let private transExpr ast nest env =
  match ast with
  | Expr.Num value -> emitf "  pushq $%d\n" value

  | Expr.Var v ->
    transVar v nest env

    // %rax に変数へのアドレスが入っているので、そこから値を読み出してプッシュする
    emitf "  movq (%%rax), %%rax\n"
    emitf "  pushq %%rax\n"

  | Expr.Str _ -> unreachable ()

  | Expr.Call ("+", [ left; right ]) ->
    emitf "  # (+)\n"
    transExpr left nest env
    transExpr right nest env

    // 右辺の値をポップする
    emitf "  popq %%rax\n"

    // スタックのトップにある値 (左辺の値) に、右辺の値を加算する
    emitf "  addq %%rax, (%%rsp)\n"

  | Expr.Call ("-", [ left; right ]) ->
    emitf "  # (-)\n"
    transExpr left nest env
    transExpr right nest env
    emitf "  popq %%rax\n"
    emitf "  subq %%rax, (%%rsp)\n"

  | Expr.Call ("*", [ left; right ]) ->
    emitf "  # ( * )\n"
    transExpr left nest env
    transExpr right nest env
    emitf "  popq %%rax\n"
    emitf "  imulq %%rax, (%%rsp)\n"

  | Expr.Call ("/", [ left; right ]) ->
    emitf "  # (/)\n"
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
    emitf "  # (~-)\n"
    transExpr arg nest env

    // スタックトップの値の符号を反転する
    emitf "  negq (%%rsp)\n"

  | Expr.Call (name, args) ->
    let tEnv = [] // 使われない
    transStmt (Stmt.CallProc(name, args)) nest tEnv env

    // 呼び出し規約として、関数呼び出しの結果は%raxレジスタに格納される
    emitf "  pushq %%rax\n"

/// %raxレジスタに参照されるアドレスを入れる
let private transVar ast nest env =
  match ast with
  | Var name ->
    let vi: VarInfo =
      match lookup name env with
      | Some (Entry.Var vi) -> vi
      | _ -> unreachable ()

    emitf "  # var(%s)\n" name

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
    emitf "  # %s[_]\n" name

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

/// `new` で確保するバッファのサイズを計算する
let private calcSize ty =
  match ty with
  | Ty.Int -> 8
  | Ty.Array (len, _) -> 8 * len
  | _ -> unreachable ()

/// 文を評価するコードを生成する
let private transStmt ast nest tEnv env =
  typeStmt ast env

  match ast with
  | Stmt.Assign (v, e) ->
    emitf "  # assign\n"

    // 右辺を評価する。その値がスタックのトップに入るので、後でポップする
    transExpr e nest env
    transVar v nest env

    // 左辺が指すアドレスが%raxに入っていて、そこに値をポップする。
    // ポップされる値は前述の通り右辺の値なので、これだけで代入になる
    emitf "  popq (%%rax)\n"

  | Stmt.CallProc ("return", [ arg ]) ->
    emitf "  # return\n"
    transExpr arg nest env

    // 結果の値がスタックの一番上に置かれているので、それを%raxに入れる
    // (関数の結果は%raxレジスタに入れて返す)
    emitf "  popq %%rax\n"

  | Stmt.CallProc ("new", [ Expr.Var v ]) ->
    emitf "  # new\n"
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
    emitf "  # scan\n"

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
    emitf "  # sprint\n"

    // 適当に一意な整数値。前にLをつけたものを文字列データのラベルに使う
    let id = freshLabel ()

    // 文字列を静的領域に配置する
    emitf "  .data\n"
    emitf "L%d:\n" id
    emitf "  .string \"%s\"\n" value

    emitf "  .text\n"
    emitf "  leaq L%d(%%rip), %%rdi\n" id
    emitf "  movq $0, %%rax\n"
    emitf "  callq printf\n"

  | Stmt.CallProc (name, args) ->
    let arity = List.length args
    emitf "  # call(%s/%d)\n" name arity

    // スタックのアラインメントの制約を守るため、
    // 引数領域と静的リンクを積む領域のサイズが16の倍数にならない場合、
    // 余分な整数値をプッシュして16の倍数にする
    if (arity + 1) % 2 = 1 then
      emitf "  pushq $0\n"

    // 引数は後方から順に積む
    for arg in args |> List.rev do
      transExpr arg nest env

    match name with
    | "iprint" ->
      emitf "  # iprint\n"

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

      emitf "  callq printf\n" // (引数をポップしなくてよい？)

    | _ ->
      let fi =
        match lookup name env with
        | Some (Entry.Fun fi) -> fi
        | _ -> unreachable ()

      // 静的リンクを渡す
      if nest >= fi.Level then
        // いまのフレームからはじめて呼び出そうとしている関数の親にあたるフレームのアドレスを%raxにいれる
        emitf "  movq %%rbp, %%rax\n"

        for _ in 0 .. nest - fi.Level do
          emitf "  movq 16(%%rax), %%rax\n"

        // フレームへのリンクをスタックに積む
        emitf "  pushq %%rax\n"
      else
        emitf "  pushq $0\n"

      emitf "  callq %s\n" name

      // 引数と静的リンクを下ろす
      emitf "  addq $%d, %%rsp\n" ((arity + 2) / 2 * 16)

  | Stmt.Block (decs, stmts) ->
    emitf "  # block\n"
    let tEnv, env, addr = typeDecs decs nest tEnv env

    for dec in decs do
      transDec dec nest tEnv env

    emitf "  # frame\n"

    // フレームを拡張する
    // 16バイトアラインメントを保つためサイズは16の倍数に切り上げる
    let frameSize = (-addr + 16) / 16 * 16
    emitf "  subq $%d, %%rsp\n" frameSize

    for stmt in stmts do
      transStmt stmt nest tEnv env

  | Stmt.If (e, s, None) ->
    // else節がないとき:
    //    if (e) s;
    // ==>
    //    if not e then goto L_ENDIF
    //    s
    // L_ENDIF: ;

    emitf "  # if\n"
    let l = transCond e nest env
    transStmt s nest tEnv env
    emitf "L%d:        # endif\n" l

  | Stmt.If (e, s1, Some s2) ->
    // elseがあるとき:
    //    if (e) s1; else s2;
    // ==>
    //    if not e then goto L_ELSE
    //    s1
    //    goto L_ENDIF
    // L_ELSE:
    //    s2
    // L_ENDIF: ;

    emitf "  # if-else\n"
    let l1 = transCond e nest env
    let l2 = freshLabel ()

    emitf "  # then\n"
    transStmt s1 nest tEnv env
    emitf "  jmp L%d\n" l2

    emitf "  # else\n"
    emitf "L%d:\n" l1
    transStmt s2 nest tEnv env
    emitf "L%d:\n" l2

  | Stmt.While (e, s) ->
    //    while (e) s;
    // ==>
    // L_CONTINUE:
    //    if not e then goto L_BREAK
    //    s
    //    goto L_CONTINUE
    // L_BREAK: ;

    emitf "  # while\n"
    let l1 = freshLabel ()
    emitf "L%d:\n" l1
    let l2 = transCond e nest env
    transStmt s nest tEnv env
    emitf "  jmp L%d\n" l1
    emitf "L%d:\n" l2

  | Stmt.Nil -> ()

/// 条件式のコード生成を行う
///
/// 条件が満たさなかったらジャンプするような命令列を出力し、
/// ジャンプ先のラベル番号を返す
let private transCond ast nest env =
  let dest = freshLabel ()

  let name, left, right =
    match ast with
    | Expr.Call (name, [ left; right ]) -> name, left, right
    | _ -> unreachable ()

  // 関係が成り立たないときにジャンプする命令の名前
  let jump =
    match name with
    | "==" -> "jne"
    | "!=" -> "je"
    | "<" -> "jge"
    | "<=" -> "jg"
    | ">" -> "jle"
    | ">=" -> "jl"
    | _ -> unreachable ()

  transExpr left nest env
  transExpr right nest env
  emitf "  popq %%rax\n"
  emitf "  popq %%rbx\n"
  emitf "  cmpq %%rax, %%rbx\n"
  emitf "  %s L%d\n" jump dest
  dest

let private prologue =
  """  # prologue
  pushq %rbp
  movq %rsp, %rbp"""

let private epilogue =
  """  # epilogue
  leaveq
  retq"""

let private transDec ast nest tEnv env =
  match ast with
  | Dec.Type (name, typ) ->
    let tyOptRef =
      match lookup name tEnv with
      | Some (Ty.Name (_, r)) -> r
      | _ -> unreachable ()

    let ty = createTy tEnv typ
    tyOptRef.contents <- Some ty

  | Dec.Var _ -> ()

  | Dec.Func (name, fargs, _, block) ->
    let parent = sCode.contents
    sCode.contents <- StringBuilder()

    emitf "# ==== func ====\n"
    emitf "%s:\n" name
    emitf "%s\n" prologue

    let localEnv = typeParamDec fargs (nest + 1) tEnv env
    transStmt block (nest + 1) tEnv localEnv

    emitf "%s\n" epilogue

    let code = sCode.contents.ToString()
    sOutput.Append("\n").Append(code) |> ignore

    sCode.contents <- parent

let codeGen ast =
  sCode.contents.Clear() |> ignore
  sOutput.Clear() |> ignore

  emitf "# Generated by simple compiler.\n\n"

  // scan, iprint で使う書式文字列
  emitf "IO: .string \"%%lld\"\n"

  emitf "    .text\n"

  // main関数
  emitf "  .globl main\n"
  emitf "main:\n"
  emitf "%s\n" prologue
  emitf "  # ==== toplevel ====\n"
  transStmt ast 0 (builtInTEnv ()) (builtInEnv ())
  emitf "  movq $0, %%rax\n"
  emitf "%s\n" epilogue

  // 関数の定義を書き足す
  emitf "%s" (sOutput.ToString())

  sCode.contents.ToString()
