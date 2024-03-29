// Signature file for parser generated by fsyacc
module Parser
type token = 
  | EOF
  | SEMI
  | COMMA
  | RP
  | LP
  | RS
  | LS
  | RB
  | LB
  | DIV
  | TIMES
  | MINUS
  | PLUS
  | LE
  | GE
  | LT
  | GT
  | NEQ
  | EQ
  | ASSIGN
  | STR of (string)
  | ID of (string)
  | VOID
  | TYPE
  | RETURN
  | NEW
  | INT
  | IPRINT
  | SPRINT
  | SCAN
  | WHILE
  | ELSE
  | IF
  | NUM of (string)
type tokenId = 
    | TOKEN_EOF
    | TOKEN_SEMI
    | TOKEN_COMMA
    | TOKEN_RP
    | TOKEN_LP
    | TOKEN_RS
    | TOKEN_LS
    | TOKEN_RB
    | TOKEN_LB
    | TOKEN_DIV
    | TOKEN_TIMES
    | TOKEN_MINUS
    | TOKEN_PLUS
    | TOKEN_LE
    | TOKEN_GE
    | TOKEN_LT
    | TOKEN_GT
    | TOKEN_NEQ
    | TOKEN_EQ
    | TOKEN_ASSIGN
    | TOKEN_STR
    | TOKEN_ID
    | TOKEN_VOID
    | TOKEN_TYPE
    | TOKEN_RETURN
    | TOKEN_NEW
    | TOKEN_INT
    | TOKEN_IPRINT
    | TOKEN_SPRINT
    | TOKEN_SCAN
    | TOKEN_WHILE
    | TOKEN_ELSE
    | TOKEN_IF
    | TOKEN_NUM
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startprog
    | NONTERM_prog
    | NONTERM_ty
    | NONTERM_decs
    | NONTERM_dec
    | NONTERM_ids
    | NONTERM_fargs_opt
    | NONTERM_fargs
    | NONTERM_stmts
    | NONTERM_stmt
    | NONTERM_aargs_opt
    | NONTERM_aargs
    | NONTERM_block
    | NONTERM_expr
    | NONTERM_cond
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val prog : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (Stmt) 
