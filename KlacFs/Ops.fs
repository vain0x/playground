module Ops

    open FParsec

    let opp = OperatorPrecedenceParser<AST.Expr, Position, unit>()
    let p_after_str_ref = ref (FParsec.CharParsers.getPosition .>> CharParsers.spaces)
    
    let reg_infix_op assoc op_name precedence =
        opp.AddOperator <|
            InfixOperator
                (op_name, ! p_after_str_ref, precedence, assoc,
                fun lhs rhs -> AST.Expr.AppIn (AST.Ident op_name, lhs, rhs))
    let reg_infixl_op = reg_infix_op Associativity.Left
    let reg_infixr_op = reg_infix_op Associativity.Right

    ///中置演算子として登録済みの名前であるか？
    let is_infix_op op_name =
        opp.Operators |> Seq.exists (fun op ->
            op.Type = OperatorType.Infix
            && op.String = op_name)

    ///演算式解析器に、項パーサと組み込み演算子の情報を登録する
    ///各種パーサを用意した後に実行される
    let internal init_opp pterm p_after_str =
        opp.TermParser <- pterm
        p_after_str_ref := p_after_str

        reg_infixl_op ";" 1
        reg_infixl_op "-" 6
        reg_infixr_op "^" 8

    ()
