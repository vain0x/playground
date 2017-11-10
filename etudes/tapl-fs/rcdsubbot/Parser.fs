[<AutoOpen>]
module Parser

open System
open System.IO
open FParsec
open FParsec.Primitives
open FParsec.CharParsers
open Support.Error

let tmcurry2 f fi x y = f (fi, x, y)

let add_index i = function
  | ("", v) -> (string i, v)
  | (k, v) -> (k, v)

let getFI = parse {
    let! pos = getPosition
    return FI (pos.StreamName, pos.Line |> int, pos.Column |> int)
  }
  
let poperatorl pl pr (pdelim: Parser<unit, _>) f = parse {
    let! x = attempt pl
    let! xs = many (attempt (pdelim >>. pr))
    return List.fold f x xs
  }

let pIdent =
  let isIdentFirstChar c = isLetter c || c = '_'
  let isIdentChar c      = isIdentFirstChar c || isDigit c
  (many1Satisfy2L isIdentFirstChar isIdentChar "identifier") .>> spaces
  
// レコード的な構文のパーサ
// pdelim: ラベルと value 部の区切り (型なら ':', 値なら '=')
// pvalue: value 部のパーサ
let pRecord delim pvalue =
  let pKeyValueField = parse {
      let! f = pIdent
      do! spaces .>> skipChar delim .>> spaces
      let! v = pvalue
      return (f, v)
    }
  // ラベルなしのフィールド (位置に応じて番号が割り振られる)
  let pIndexValueField =
    pvalue |>> (fun v -> ("", v))

  let pField =
        attempt pKeyValueField
    <|> pIndexValueField
  let pFields =
    sepBy (spaces >>. pField .>> spaces) (skipChar ',')
    |>> List.mapi add_index

  between (skipChar '{') (skipChar '}') (spaces >>. pFields)
  
let (pType, pTypeRef) = createParserForwardedToRef ()
let (pAtomType, pAtomTypeRef) = createParserForwardedToRef ()

let pArrowType =
  chainr1 (spaces >>. pAtomType .>> spaces) (skipString "->" >>% curry2 TyArr)

let pRecordType =
  pRecord ':' pType |>> TyRecord

let pKeywordType name tag =
  skipString name >>% tag

pAtomTypeRef :=
      attempt (between (skipChar '(') (skipChar ')') pType)
  <|> pRecordType
  <|> pKeywordType "Top" TyTop
  <|> pKeywordType "Bot" TyBot
  <|> pKeywordType "Unit" TyUnit

pTypeRef :=
      attempt (spaces >>. pArrowType)
  <|> pAtomType

let (pTerm, pTermRef) = createParserForwardedToRef ()
let (pAtomTerm, pAtomTermRef) = createParserForwardedToRef ()

let pTmConst name value =
  getFI >>= fun fi ->
    attempt (skipString name)
    |>> konst (value fi)

let pTmVar = parse {
    let! fi = getFI
    let! name = pIdent .>> spaces
    let! ctx = getUserState
    let index = name2index fi ctx name
    return TmVar (fi, index, ctxlength ctx)
  }
  
let pTmRecord =
  getFI >>= fun fi ->
    pRecord '=' pTerm |>> fun fields -> TmRecord (fi, fields)

let pTmAbs =
  let pLambda =
    skipString "λ"
    <|> skipString "lambda"
    <|> skipString "\\"
  parse {
    let! fi = getFI
    do! attempt pLambda .>> spaces
    let! name = pIdent
    do! spaces >>. skipChar ':' .>> spaces
    let! typ = pType
    do! spaces >>. skipChar '.' .>> spaces
    let! outer_ctx = getUserState

    // 変数を束縛
    do! updateUserState (addname name)
    let! term = pTerm

    do! setUserState outer_ctx
    return TmAbs (fi, name, typ, term)
  }

let pTmProj =
  let pField = (pint32 |>> string) <|> pIdent
  getFI >>= fun fi ->
    poperatorl pAtomTerm pField (spaces >>. skipChar '.') (tmcurry2 TmProj fi)

let pTmApp =
  getFI >>= fun fi ->
    poperatorl pTmProj pTmProj spaces (tmcurry2 TmApp fi)

// (t1; t2) ≡ ((\_: Unit. t2) t1)
let pTermSeq =
  let combine fi t1 t2 =
    TmApp (fi, TmAbs (fi, "_", TyUnit, t2), t1)
  chainr1 (spaces >>. pTerm) (spaces >>. skipChar ';' >>. getFI |>> combine)

let pParenTermSeq =
  between (skipChar '(') (skipChar ')') (pTermSeq .>> spaces)

pAtomTermRef :=
      attempt pParenTermSeq
  <|> pTmConst "unit" TmUnit
  <|> pTmVar
  <|> pTmRecord

pTermRef :=
      pTmAbs
  <|> pTmApp

let pExpr = pTermSeq .>> spaces .>> eof

let parseExpr stream_name input =
  match runParserOnString pExpr emptycontext stream_name input with
  | ParserResult.Success (res, ctx, pos) ->
      (res, ctx)
  | ParserResult.Failure (err_msg, err, ctx) ->
      failwith err_msg 
