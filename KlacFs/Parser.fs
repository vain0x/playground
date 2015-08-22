module Parser

    #nowarn "40"

    open System
    open System.Text
    open System.Collections.Generic
    open FParsec
    open FParsec.Primitives
    open FParsec.CharParsers
    open Basis.Core
    open Util
    open AST
    
    type internal Parser<'a> = Parser<'a, unit>
    
    ///解析結果を取り出す
    let internal get_parse_result (res : ParserResult<_,_>) =
        match res with
        | ParserResult.Success (x, _, _) ->
            Success x
        | ParserResult.Failure (s, err, state) ->
            Failure <| string err

    let internal one_line_comment =
        let endp = skipNewline <|> eof
        skipString "//" >>. skipManyTill anyChar endp

    let internal nested_comment, nested_comment_ref = createParserForwardedToRef ()
    nested_comment_ref :=
        let (beg_str, end_str) = ("/+", "+/")
        skipString beg_str >>. skipManyTill (nested_comment <|> skipAnyChar) (skipString end_str)
    ;

    let internal block_comment =
        let (beg_str, end_str) = ("/*", "*/")
        skipString beg_str >>. (skipManyTill anyChar <| skipString end_str)

    let internal comment: Parser<_> =
            one_line_comment
        <|> nested_comment
        <|> block_comment
        
    let internal klac_spaces: Parser<_> =
        skipMany (spaces1 <|> comment) <|> eof

    let internal klac_spaces1: Parser<_> =
        skipMany1 (spaces1 <|> comment)

    let inline internal token p =
        klac_spaces >>. p

    let int_lit: Parser<_> =
        parse {
            let! digits =
                many1Chars2 digit (digit <|> pchar '_')
                |>> Str.filter Char.IsDigit
            let (succeeds, value) = System.Int32.TryParse digits
            if succeeds then
                return AST.IntLit value
            }

    let regular_ident: Parser<_> =
        let p_let_init = letter <|> pchar '_'
        let p_let_body = p_let_init <|> digit
        many1Chars2 p_let_init p_let_body

    let generalized_ident: Parser<_> =
        let double_backquotes = skipString "``"
        double_backquotes >>. (many1CharsTill anyChar double_backquotes)

    let ident = 
        (regular_ident <|> generalized_ident)
        |>> AST.Ident

    let internal ident_ptn: Parser<_> =
        skipChar '\\' >>= konst_unit ident

    let internal op_token: Parser<_> =
        many1Chars <| anyOf "+-*/%&|^~<>=!?:@$"

    let internal empty: Parser<_> =
        preturn AST.Expr.Nothing

    ///Klacの式
    let internal expr, expr_ref = createParserForwardedToRef ()

    ///配列リテラル、またはグループ式
    let internal array_lit =
        let delimiter      = token <| skipChar ','
        let last_delimiter = opt delimiter |>> Option.isSome
        ///splat の左側にある項の列
        let left  = sepBy1 (token <| expr) delimiter
        let splat_elem =
            delimiter
            >>. opt (token expr .>> (token <| skipString ".."))
        ///splat の右側にある項の列、および最後のカンマ
        let right = tuple2 left last_delimiter
        let body =
            tuple3 left splat_elem right
            |>> function
                | [e], None, ([], false) -> e
                | es1, es2,  (es3, _)    -> AST.Array (es1 |> Array.ofList, es2, es3 |> Array.ofList)

        between (skipChar '(') (skipChar ')') body

    let internal atomic_term =
        int_lit
        <|> ident
        <|> array_lit

    ///関数適用式
    let internal app_pr =
        sepEndBy1 atomic_term klac_spaces1
        |>> List.fold1 (curry2 AST.AppPr)
        |>> Option.get //Never None here because sepEndBy1 parses at least one value.

    let internal term =
        app_pr

    do
        let p_term = token term .>> klac_spaces
        let p_after_str = getPosition .>> klac_spaces
        Ops.init_opp p_term p_after_str

    expr_ref := Ops.opp.ExpressionParser

    let klac_parser: Parser<_> =
        token (expr <|> empty)
        .>> token eof

    let parse_klac_source s =
        s
        |> run (klac_parser)
        |> get_parse_result
