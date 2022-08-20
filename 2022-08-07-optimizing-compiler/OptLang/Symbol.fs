module rec OptLang.Symbol

[<RequireQualifiedAccess; CustomEquality; CustomComparison>]
[<StructuredFormatDisplay("{Kind}{Index}:{Name}")>]
type Symbol =
  { Kind: string
    Index: int
    Name: string }
  override this.ToString() = $"{this.Kind}{this.Index}:{this.Name}"

  override this.Equals(obj) =
    match obj with
    | :? Symbol as other -> this.Index = other.Index
    | _ -> false

  override this.GetHashCode() = this.Index.GetHashCode()

  interface System.IComparable with
    override this.CompareTo(obj) =
      this.Index.CompareTo((obj :?> Symbol).Index)

  interface System.IComparable<Symbol> with
    override this.CompareTo(other) = this.Index.CompareTo(other.Index)

let newSymbol kind index name : Symbol =
  { Kind = kind
    Index = index
    Name = name }
