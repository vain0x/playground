module Otojson.Json

  open Microsoft.FSharp.Reflection
  open System
  open System.Collections.Generic

  [<RequireQualifiedAccess>]
  type JsonValue =
    | Null
    | Boolean of bool
    | Number of float
    | String of string
    | Array of list<JsonValue>
    | Object of list<string * JsonValue>

  type Representer =
    Type -> obj -> JsonValue

  type TypeDefinition =
    {
      Accept: Type -> bool
      Represent: Representer -> Representer
    }

  module TypeDefinitions =
    open Microsoft.FSharp.Reflection
    open System
    open System.Collections.Generic

    module Primities =
      let private representBy<'T> (toString: 'T -> string) (_: Representer) (_: Type) (value: obj) =
        JsonValue.String (toString (unbox<'T> value))

      let representSeq<'T> (represent: Representer) (ty: Type) (value: obj) =
        JsonValue.Array [
          for value in value :?> seq<'T>

          let elementType = RuntimeSeq.elementType t
          let values = RuntimeSeq.map (represent elementType) t obj |> Seq.toList
          Sequence (values, None)

      let private simpleDef<'T> =
        {
          Accept = (=) typeof<'T>
          Represent = representBy string
        }

      let intDef = simpleDef<int>
      let int64Def = simpleDef<int64>
      let stringDef = simpleDef<string>
      let decimalDef = simpleDef<decimal>

      let boolDef =
        {
          Accept = (=) typeof<bool>
          Represent = fun _ _ value ->
            JsonValue.Boolean(value :?> bool)
        }

      let floatDef =
        {
          Accept = (=) typeof<float>
          Represent = fun _ _ value ->
            let value = unbox<float> value
            if Double.IsNaN(value) || Double.IsInfinity(value)
            then JsonValue.Null
            else JsonValue.Number value
        }

      /// Represents DateTime value as string in ISO 8601 format (yyyy-MM-ddTHH:mm:ss.ffffffZ).
      let dateTimeDef =
        {
          Accept = (=) typeof<DateTime>
          Represent = fun _ _ value ->
            let dateTime = unbox<DateTime> value
            JsonValue.String (dateTime.ToString("o"))
        }

      let timeSpanDef =
        {
          Accept = (=) typeof<TimeSpan>
          Represent = fun _ _ value ->
            let timeSpan = unbox<TimeSpan> value
            JsonValue.String (timeSpan.ToString(@"hh\:mm\:ss\.fff"))
        }

      let recordDef =
        {
          Accept = fun ty -> FSharpType.IsRecord(ty)
          Represent = fun represent ty value ->
            JsonValue.Object [
              for propertyInfo in FSharpType.GetRecordFields(ty) ->
                let jsonValue = represent propertyInfo.PropertyType (propertyInfo.GetValue(value))
                (propertyInfo.Name, jsonValue)
            ]
        }

      let tupleDef =
        {
          Accept = fun ty -> FSharpType.IsTuple(ty)
          Represent = fun represent ty value ->
            JsonValue.Array [
              let itemTypes = FSharpType.GetTupleElements(ty)
              let itemValues = FSharpValue.GetTupleFields(value)
              for (itemType, itemValue) in Seq.zip itemTypes itemValues ->
                represent itemType itemValue
            ]
        }

      let listDef =

    module internal Detail =
      module RecordRepresenter =
        open System.Reflection
        open RecordConstructor

      let listDef = {
        Accept = (isGenericTypeDef typedefof<list<_>>)
        Represent = representSeqAsSequence
      }

      let setDef = {
        Accept = (isGenericTypeDef typedefof<Set<_>>)
        Construct = fun construct' t yaml ->
          match yaml with
          | Sequence (sequence, _) ->
            let elementType = t.GetGenericArguments().[0]
            let elements = sequence |> List.map (construct' elementType)
            ObjectElementSeq.toSet elementType elements
          | otherwise -> raise (mustBeSequence t otherwise)
        Represent = representSeqAsSequence
      }

      let mapDef = {
        Accept = (isGenericTypeDef typedefof<Map<_, _>>)
        Construct = fun construct' t yaml ->
          match yaml with
          | Mapping (mapping, _) ->
            let keyType, valueType = let ts = t.GetGenericArguments() in (ts.[0], ts.[1])
            let values =
              mapping
              |> Seq.map (fun (KeyValue (keyYaml, valueYaml)) ->
                let key = construct' keyType keyYaml
                let value = construct' valueType valueYaml
                (key, value)
              )
            ObjectElementSeq.toMap keyType valueType values
          | otherwise -> raise (mustBeMapping t otherwise)
        Represent = fun represent t obj ->
          let keyType, valueType = RuntimeMap.elementTypes t
          let values =
            RuntimeMap.toSeq t obj
            |> Seq.map (fun (key, value) ->
              let key =
                match represent keyType key with
                | Scalar _ as s -> s
                | otherwise -> raise (FsYamlException.Create(Resources.getString "mapKeyMustBeScalar", Type.print t, YamlObject.nodeTypeName otherwise))
              let value = represent valueType value
              (key, value)
            )
            |> ArrayMap.ofSeq
          Mapping (values, None)
      }

      let arrayMapDef = {
        Accept = fun typ ->
          typ.IsArray
          && typ.GetElementType() |> isGenericTypeDef typedefof<KeyValuePair<_, _>>
        Construct = fun construct' typ yaml ->
          match yaml with
          | Mapping (mapping, _) ->
            let (keyType, valueType) =
              let types = typ.GetElementType().GetGenericArguments()
              (types.[0], types.[1])
            let pairs =
              mapping |> Seq.map
                (fun (KeyValue (keyYaml, valueYaml)) ->
                  let keyObj = construct' keyType keyYaml
                  let valueObj = construct' valueType valueYaml
                  (keyObj, valueObj)
                )
            ObjectElementSeq.toArrayMap keyType valueType pairs
          | otherwise -> raise (mustBeMapping typ otherwise)
        Represent = fun represent typ obj ->
          let (keyType, valueType) = RuntimeArrayMap.elementTypes typ
          let values =
            RuntimeArrayMap.toSeq typ obj
            |> Seq.map (fun (key, value) ->
              let key =
                match represent keyType key with
                | Scalar _ as s -> s
                | keyYaml -> mustBeScalar keyType keyYaml |> raise
              let value = represent valueType value
              (key, value)
            )
            |> ArrayMap.ofSeq
          Mapping (values, None)
      }

      let arrayDef = {
        Accept = (fun t -> t.IsArray)
        Construct = fun construct' t yaml ->
          match yaml with
          | Sequence (sequence, _) ->
            let elementType = t.GetElementType()
            let values = Seq.map (construct' elementType) sequence
            ObjectElementSeq.toArray elementType values
          | otherwise -> raise (mustBeSequence t otherwise)
        Represent = representSeqAsSequence
      }

      let seqDef = {
        Accept = (isGenericTypeDef typedefof<seq<_>>)
        Construct = fun construct' t yaml ->
          match yaml with
          | Sequence (sequence, _) ->
            let elementType = t.GetGenericArguments().[0]
            let xs = Seq.map (construct' elementType) sequence
            ObjectElementSeq.cast elementType xs
          | otherwise -> raise (mustBeSequence t otherwise)
        Represent = representSeqAsSequence
      }

      module UnionConstructor =
        let makeUnion (union: UnionCaseInfo) values = FSharpValue.MakeUnion(union, Seq.toArray values)

        let noFieldCase yaml (union: UnionCaseInfo) =
          match yaml with
          | Scalar (scalar, _) ->
            let name = Scalar.value scalar
            if name = union.Name then
              Some (makeUnion union [])
            else
              None
          | _ -> None

        let tryNamedFieldCase construct' (union: UnionCaseInfo) (mapping: ArrayMap<YamlObject, YamlObject>) =
          let fields = union.GetFields()
          let yamls = fields |> Array.choose (fun field -> Mapping.tryFind field.Name mapping)

          Seq.tryZip fields yamls
          |> Option.map (fun xs ->
            xs
            |> Seq.map (fun (field, yaml) -> construct' field.PropertyType yaml)
            |> makeUnion union
          )

        let caseWithFields construct' (union: UnionCaseInfo) yamls (parentYamlForExceptionMessage: YamlObject) =
          let fieldTypes = union.GetFields()
          let fieldValues =
            match Seq.tryZip fieldTypes yamls with
            | Some xs -> xs |> Seq.map (fun (t, yaml) -> construct' t.PropertyType yaml) |> Seq.toArray
            | None -> raise (FsYamlException.WithYaml(parentYamlForExceptionMessage, Resources.getString "unionCaseElementNumber", (Union.printCase union), fieldTypes.Length))
          makeUnion union fieldValues

        let oneFieldCase construct' yaml (union: UnionCaseInfo) =
          match yaml with
          | Mapping (mapping, _) ->
            Mapping.tryFind union.Name mapping
            |> Option.bind (fun value ->
              let maybeNamedField =
                match value with
                | Mapping (mapping, _) -> tryNamedFieldCase construct' union mapping
                | _ -> None
              match maybeNamedField with
              | Some named -> Some named
              | None -> Some (caseWithFields construct' union [ value ] yaml)
            )
          | _ -> None

        let manyFieldsCase construct' yaml (union: UnionCaseInfo) =
          match yaml with
          | Mapping (mapping, _) ->
            Mapping.tryFind union.Name mapping
            |> Option.bind (function
              | Sequence (sequence, _) -> Some (caseWithFields construct' union sequence yaml)
              | Mapping (mapping, _) -> tryNamedFieldCase construct' union mapping
              | _ -> None
            )
          | _ -> None

        let tryConstruct construct' yaml (union: UnionCaseInfo) =
          let fields = union.GetFields()
          match fields.Length with
          | 0 -> noFieldCase yaml union
          | 1 -> oneFieldCase construct' yaml union
          | _ -> manyFieldsCase construct' yaml union

      module UnionRepresenter =
        let caseName (union: UnionCaseInfo) = Scalar (Plain union.Name, None)

        let oneField represent (union: UnionCaseInfo) (value: obj) =
          let fields = union.GetFields()
          let valueType = fields.[0].PropertyType
          let value = represent valueType value
          Mapping (ArrayMap.singleton (caseName union) value, None)

        let manyFields represent (union: UnionCaseInfo) (values: obj[]) =
          let fields = union.GetFields()
          let fieldValues =
            let xs =
              Seq.zip fields values
              |> Seq.map (fun (field, value) -> represent field.PropertyType value)
              |> Seq.toList
            Sequence (xs, None)
          Mapping (ArrayMap.singleton (caseName union) fieldValues, None)

        let isNamedFieldCase (union: UnionCaseInfo) =
          let fields = union.GetFields()
          match fields.Length with
          | 0 -> false
          | 1 -> fields.[0].Name <> "Item"
          | _ -> fields |> Array.mapi (fun i field -> (i + 1, field)) |> Array.forall (fun (n, field) -> field.Name <> sprintf "Item%d" n)

        let namedField represent (union: UnionCaseInfo) (values: obj[]) =
          let fields = union.GetFields()
          let values =
            Seq.zip fields values
            |> Seq.map (fun (field, value) ->
              let name = Scalar (Plain field.Name, None)
              let value = represent field.PropertyType value
              (name, value)
            )
            |> ArrayMap.ofSeq
          let name = caseName union
          let fieldMapping = Mapping (values, None)
          Mapping (ArrayMap.singleton name fieldMapping, None)

        let represent (represent: RecursiveRepresenter) (t: Type) (obj: obj) =
          let union, values = FSharpValue.GetUnionFields(obj, t)
          if isNamedFieldCase union then
            namedField represent union values
          else
            match values.Length with
            | 0 -> caseName union
            | 1 -> oneField represent union values.[0]
            | _ -> manyFields represent union values

      let unionDef = {
        Accept = fun t -> FSharpType.IsUnion(t)
        Construct = fun construct' t yaml ->
          match FSharpType.GetUnionCases(t) |> Seq.tryPick (UnionConstructor.tryConstruct construct' yaml) with
          | Some x -> x
          | None -> raise (FsYamlException.WithYaml(yaml, Resources.getString "unionCaseNotFound", Type.print t))
        Represent = UnionRepresenter.represent
      }

      let optionDef = {
        Accept = fun t -> FSharpType.IsUnion(t) && isGenericTypeDef typedefof<Option<_>> t
        Construct = fun construct' t yaml ->
          let noneCase, someCase = let xs = FSharpType.GetUnionCases(t) in (xs.[0], xs.[1])
          match yaml with
          | Null _ -> (UnionConstructor.makeUnion noneCase [])
          | _ ->
            try
              let parameterType = t.GetGenericArguments().[0]
              let value = construct' parameterType yaml
              UnionConstructor.makeUnion someCase [ value ]
            with _ ->  unionDef.Construct construct' t yaml
        Represent = fun represent t obj ->
          match obj with
          | null -> Null None
          | _ ->
            let caseInfo, values = FSharpValue.GetUnionFields(obj, t)
            let valueType = caseInfo.GetFields().[0].PropertyType
            represent valueType values.[0]
      }

    open Detail

    let internal defaultDefinitions = [ intDef; int64Def; floatDef; stringDef; boolDef; decimalDef; datetimeDef; timespanDef; recordDef; tupleDef; listDef; setDef; mapDef; arrayMapDef; arrayDef; seqDef; optionDef; unionDef ]


  let stringify indent =
  let hello name =
      printfn "Hello %s" name
