module Value

    type Object =
        | Thunk of AST.Expr * Lazy<Object>
        | Bool of bool
        | Int of int
        | IdentPtn of string
        | List of (Object list)
        | Dict of Map<string, Object>
        | InternalFunc of (Object list -> Object)
