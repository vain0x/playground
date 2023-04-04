module rec TinyAvro

open System
open System.IO
open System.Text

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type FieldDef =
  { Name: string
    Ty: Ty
  // Default: Value option
   }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type RecordDef =
  { Name: string
    // Alias: string list
    Fields: FieldDef list }

// boolean, long, float(f32), double(f64), bytes
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Ty =
  | Null
  | Int
  | String
  | Name of string
  | Union of Ty list

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Value =
  | Null
  | Int of int
  | String of string

module BinaryDecoding =
  let readInt (reader: BinaryReader) =
    let z =
      let b1 = reader.ReadByte()
      if (b1 &&& 0x80uy) = 0uy then
        uint b1
      else
        let b1 = uint b1 // (b1 &&& 0x7Fuy)
        let b2 = reader.ReadByte()
        if (b2 &&& 0x80uy) = 0uy then
          (uint b2 <<< 8) ||| b1
        else
          // let b = (uint (b2 &&& 0x7Fuy) <<< 8) ||| b1
          let b = (uint b2 <<< 8) ||| b1
          let b3 = reader.ReadByte()
          if (b3 &&& 0x80uy) = 0uy then
            (uint b3 <<< 16) ||| b
          else
            let b4 = reader.ReadByte()
            (uint b4 <<< 24) ||| (uint b3 <<< 16) ||| b

    if (z &&& 1u) <> 0u then
      -(int (z >>> 1))
    else
      int (z >>> 1)

  let readString (reader: BinaryReader) =
    let n = readInt reader
    Encoding.UTF8.GetString(reader.ReadBytes(n))

module BinaryEncoding =
  let writeInt (writer: BinaryWriter) (n: int) =
    // zig-zag
    let z = if n >= 0 then uint n <<< 1 else ((uint -n) <<< 1) ||| 1u
    printfn " z=%d (0x%x)" z z

    let K = 0x80uy

    if z <= 0x7Fu then
      writer.Write(byte (z &&& 0x7Fu))
    else if z <= 0x7FFFu then
      writer.Write(byte (z &&& 0x7Fu) ||| K)
      let z = z >>> 8
      writer.Write(byte (z &&& 0xFFu))
    else if z <= 0x7FFFFFu then
      writer.Write(byte (z &&& 0x7Fu) ||| K)
      let z = z >>> 8
      writer.Write(byte (z &&& 0x7Fu) ||| K)
      let z = z >>> 8
      writer.Write(byte (z &&& 0xFFu))
    else
      writer.Write(byte (z &&& 0x7Fu) ||| K)
      let z = z >>> 8
      writer.Write(byte (z &&& 0x7Fu) ||| K)
      let z = z >>> 8
      writer.Write(byte (z &&& 0x7Fu) ||| K)
      let z = z >>> 8
      writer.Write(byte (z &&& 0xFFu))

  let writeString (writer: BinaryWriter) (s: string) =
    writeInt writer s.Length
    writer.Write(System.Text.Encoding.UTF8.GetBytes(s))

module internal Tests =
  let private newFieldDef name ty : FieldDef = { Name = name; Ty = ty }

  type User =
    { Name: string
      FavoriteNumber: int option
      FavoriteColor: string option }

  let userTy: RecordDef =
    { Name = "User"
      Fields =
        [ newFieldDef "Name" Ty.String
          newFieldDef "FavoriteNumber" (Ty.Union [ Ty.Null; Ty.Int ])
          newFieldDef "FavoriteColor" (Ty.Union [ Ty.Null; Ty.String ]) ] }

  let writeUser (writer: BinaryWriter) (u: User) =
    // Name
    BinaryEncoding.writeString writer u.Name

    // FavoriteNumber
    match u.FavoriteNumber with
    | None ->
      // discriminant (0: null)
      writer.Write(0uy)
    | Some n ->
      // discriminant (1: int)
      BinaryEncoding.writeInt writer n

    // FavoriteColor
    match u.FavoriteColor with
    | None ->
      // discriminant (0: null)
      writer.Write(0uy)
    | Some s ->
      // discriminant (1: string)
      writer.Write(1uy)
      BinaryEncoding.writeString writer s

let internal tests () =
  let _ =
    let p value len =
      printfn "test (%d, 0x%x)" value value
      use stream = new MemoryStream(8)
      let writer = new BinaryWriter(stream)
      BinaryEncoding.writeInt writer value
      printfn "  write %d (%d)" value (int stream.Position)
      assert(int stream.Position = len)

      stream.Seek(0L, SeekOrigin.Begin) |> ignore
      let reader = new BinaryReader(stream)
      let other = BinaryDecoding.readInt reader
      printfn "  read %d (%d)" other (int stream.Position)
      assert(other = value)
      assert(int stream.Position = len)

    p 0 1
    p 1 1
    p (-1) 1
    p 42 1
    p 63 1
    p -63 1
    p 64 2
    p -64 2
    p 0x3fff 2
    p -0x3fff 2
    p 0x4000 3
    p -0x4000 3
    p 0x7fff 3
    p -0x7fff 3
    p 0x7fffffff 4
    p 0x80000000 4

  ()
