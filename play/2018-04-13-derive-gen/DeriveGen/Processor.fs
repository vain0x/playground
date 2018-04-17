module DeriveGen.Processor

open System
open System.Text
open Chiron
open Chiron.Inference

module CD = Chiron.Decoder
module JD = Json.Decode
module JE = Json.Encode

let templateSource = """/// <auto-generated />

using System;
using System.Collections.Generic;
{%- for u in Model.Usings -%}
using {{ u }};
{%- endfor -%}
{%- for ns in Model.Namespaces -%}

namespace {{ ns.NamespaceName }}
{
    {%- for class in ns.Classes -%}
    {%- if class != ns.Classes.first -%}

    {%- endif -%}
    {{ class.Modifiers | join: " " }} {{ class.Kind }} {{ class.ClassName }}
    {
        {%- for field in class.Fields -%}
        {{ field.Modifiers | join: " " }} {{ field.Type }} {{ field.FieldName }} {{- field.AutoAccessors }};
        {%- endfor -%}
        {%- for method in class.Methods -%}

        {%- if method.Parameters.size >= 3 -%}
        {{ method.Modifiers | join: " " }} {{ method.Type }}{% if method.MethodName != "" %} {{ method.MethodName }}{% endif %}(
            {%- for parameter in method.Parameters -%}
            {{ parameter.Type }} {{ parameter.ParameterName }} {%- if parameter != method.Parameters.last %},{% endif %}
            {%- endfor -%}
        )
        {%- else -%}
        {{ method.Modifiers | join: " " }} {{ method.Type }}{% if method.MethodName != "" %} {{ method.MethodName }}{% endif %}({%- for parameter in method.Parameters -%}{{ parameter.Type }} {{ parameter.ParameterName }} {%- if parameter != method.Parameters.last %}, {% endif %}{%- endfor -%})
        {%- endif -%}
        {
            {%- for statement in method.Statements -%}
            {{ statement }}
            {%- endfor -%}
        }
        {%- endfor -%}
    }
    {%- endfor -%}
}
{%- endfor -%}
"""

type DotLiquidHelper =
  static member CreateHash() =
    let warn (self: DotLiquid.Hash) name =
      failwithf "Field '%s' missing in env [%s]."
        name
        (self.Keys |> String.concat ";")
    DotLiquid.Hash(lambda = Func<_, _, _>(warn))

  static member HashFromJson(j: string) =
    let rec decode =
      function
      | Json.Null ->
        null :> obj
      | Json.True ->
        true :> obj
      | Json.False ->
        false :> obj
      | Json.Number number ->
        Double.Parse(number) :> obj
      | Json.String string ->
        string :> obj
      | Json.Array array ->
        array
        |> Seq.map decode
        |> Seq.toArray
        |> box
      | Json.Object obj ->
        let hash = DotLiquidHelper.CreateHash()
        for KeyValue (key, value) in obj |> JsonObject.toMap do
          hash.Add(key, decode value)
        hash |> box
    Json.parse j
    |> JsonResult.map decode
    |> JsonResult.getOrThrow

  static member LocalFromJson(j: string) =
    let hash = DotLiquidHelper.CreateHash()
    hash.Add("Model", DotLiquidHelper.HashFromJson(j))
    hash

module CodeGeneration =

  type Identifier = string
  type Modifier = string
  type TypeKind =
    | Class
    | Struct
  with
    static member ToJson(self: TypeKind) =
      match self with
      | Class -> Json.String "class"
      | Struct -> Json.String "struct"

  type FieldModel =
    {
      FieldName: Identifier
      Type: string
      Modifiers: Modifier[]
    }

  type ParameterModel =
    {
      ParameterName: Identifier
      Type: string
    }

  type MethodModel =
    {
      MethodName: Identifier
      Type: string
      Modifiers: Modifier[]
      Parameters: ParameterModel[]
      Statements: string[]
    }

  type ClassModel =
    {
      ClassName: Identifier
      Kind: TypeKind
      Modifiers: Modifier[]
      Fields: FieldModel[]
      Methods: MethodModel[]
    }

  type NamespaceModel =
    {
      NamespaceName: Identifier
      Classes: ClassModel[]
    }

  type ModuleModel =
    {
      UsingDirectives: string[]
      Namespaces: NamespaceModel[]
    }
  with
    static member ToJson(model: ModuleModel) =
      let jobj xs = xs |> Array.toList |> JsonObject.ofPropertyList |> Json.Object
      let usings =
        JE.stringArray model.UsingDirectives
      let namespaces =
        JE.array [|
          for model in model.Namespaces do
            let content =
              JE.array [|
                for model in model.Classes ->
                  let fields =
                    JE.array [|
                      for field in model.Fields ->
                        jobj [|
                          ("FieldName", JE.string field.FieldName)
                          ("Modifiers", JE.arrayWith JE.string field.Modifiers)
                          ("Type", JE.string field.Type)
                        |]
                    |]
                  let methods =
                    JE.array [|
                      for method in model.Methods ->
                        jobj [|
                          ("MethodName", JE.string method.MethodName)
                          ("Modifiers", JE.arrayWith JE.string method.Modifiers)
                          ("Type", JE.string method.Type)
                          ("Parameters", JE.arrayWith (fun p -> jobj [|("ParameterName", JE.string p.ParameterName); ("Type", JE.string p.Type)|]) method.Parameters)
                          ("Statements", JE.arrayWith JE.string method.Statements)
                        |]
                    |]
                  jobj [|
                    ("ClassName", JE.string model.ClassName)
                    ("Kind", TypeKind.ToJson(model.Kind))
                    ("Modifiers", JE.arrayWith JE.string model.Modifiers)
                    ("Fields", fields)
                    ("Methods", methods)
                  |]
              |]
            yield jobj [|
              ("NamespaceName", JE.string model.NamespaceName)
              ("Classes", content)
            |]
        |]
      jobj [|
        ("Usings", usings)
        ("Namespaces", namespaces)
      |]

  module Identifier =
    let toLowerCamelCase (ident: string) =
      if ident.Length = 0 then
        ident
      else if Char.ToLower(ident.[0]) = Char.ToUpper(ident.[0]) then
        "_" + ident
      else
        (Char.ToLower(ident.[0]) |> string) + ident.Substring(1)

  let generateCompleteConstructor className fields =
    let parameters =
      [|
        for field in fields ->
          {
            ParameterName = field.FieldName |> Identifier.toLowerCamelCase
            Type = field.Type
          }
      |]
    let statements =
      [|
        for field in fields do
          let parameterName = field.FieldName |> Identifier.toLowerCamelCase
          yield sprintf "%s = %s;" field.FieldName parameterName
      |]
    {
      MethodName = ""
      Modifiers = [|"public"|]
      Type = className
      Parameters = parameters
      Statements = statements
    }

  let generateEquality selfType typeKind fields =
    [|
      let param name ty = { ParameterName = name; Type = ty }
      // Equals(obj)
      yield {
        MethodName = "Equals"
        Modifiers = [|"public"; "override"|]
        Type = "bool"
        Parameters = [|{ParameterName = "obj"; Type = "object"}|]
        Statements =
          [|
            match typeKind with
            | Class ->
              yield sprintf "return Equals(obj as %s);" selfType
            | Struct ->
              yield sprintf "return obj is %s && Equals((%s)obj);" selfType selfType
          |]
      }
      // Equals(Self)
      yield {
        MethodName = "Equals"
        Modifiers = [|"public"|]
        Type = "bool"
        Parameters = [|{ParameterName = "other"; Type = selfType}|]
        Statements =
          [|
            let equation field =
              sprintf "%s == other.%s" field.FieldName field.FieldName
            if fields |> Array.length = 1 then
              yield sprintf "return %s;" (equation fields.[0])
            else
              yield sprintf "return %s" (equation fields.[0])
              for (i, field) in fields |> Seq.indexed |> Seq.skip 1 do
                let semicolon = if i = fields.Length - 1 then ";" else "";
                yield sprintf "    && %s%s" (equation field) semicolon
          |]
      }
      // GetHashCode(obj)
      yield {
        MethodName = "GetHashCode"
        Modifiers = [|"public"; "override"|]
        Type = "int"
        Parameters = [||]
        Statements =
          [|
            sprintf "return %s;" (fields |> Array.map (fun field -> "EqualityComparer<" + field.Type + ">.Default.GetHashCode(" + field.FieldName + ")") |> String.concat " ^ ")
          |]
      }
      // operator ==/!=
      for (name, expr) in [|("operator ==", "l.Equals(r)"); ("operator !=", "!(l == r)")|] do
        yield {
          MethodName = name
          Modifiers = [|"public"; "static"|]
          Type = "bool"
          Parameters = [|param "l" selfType; param "r" selfType|]
          Statements = [|sprintf "return %s;" expr|]
        }
    |]

module ConfigParsing =

  module G = CodeGeneration

  // FIXME: Separate code generation from decode.
  let parseInput j =
    let decodeFieldModel =
      JD.propertyListWith JD.string
      |> CD.map
        (fun fields ->
          [|
            for (fieldName, ty) in fields |> List.rev ->
              ({
                FieldName = fieldName
                Type = ty
                Modifiers = [|"public"; "readonly"|]
              }: G.FieldModel)
          |])

    let decodeClassModelContent =
      jsonDecoder {
        let! kind =
          JD.optional "kind"
          |> CD.map
            (function
            | Some "struct" ->
              G.Struct
            | Some "class"
            | None ->
              G.Class
            | Some value ->
              failwithf "Unknown kind: %s." value
            )
        let! fields =
          JD.required "fields"
          |> CD.compose decodeFieldModel
        let modifiers =
          [|
            yield "public"
            if kind = G.Class then
              yield "sealed"
            yield "partial"
          |]
        return (kind, modifiers, fields)
      }

    let decodeNamespaceModel =
      JD.propertyListWith decodeClassModelContent
      |> CD.map
        (fun classes ->
        [|
          for (className, (kind, modifiers, fields)) in classes |> List.rev do
            let methods =
              [|
                yield CodeGeneration.generateCompleteConstructor className fields
                yield! CodeGeneration.generateEquality className kind fields
              |]
            yield
              ({
                ClassName = className
                Kind = kind
                Modifiers = modifiers
                Fields = fields
                Methods = methods
              }: G.ClassModel)
        |])

    let rec decode = jsonDecoder {
      let! usings =
        JD.optionalArray JD.stringArray "usings"
      let! types =
        JD.required "types"
        |> CD.compose (JD.propertyListWith decodeNamespaceModel)
        |> CD.map
          (fun namespaces ->
            [|
              for (namespaceName, classes) in namespaces |> List.rev ->
                ({
                  NamespaceName = namespaceName
                  Classes = classes
                }: G.NamespaceModel)
            |])
      return
        ({
          UsingDirectives = usings
          Namespaces = types
        }: G.ModuleModel)
    }

    Json.parse j
    |> JsonResult.bind decode

let render (model: CodeGeneration.ModuleModel) =
  let template = DotLiquid.Template.Parse(templateSource)
  let modelJson = Json.serialize model
  let local = DotLiquidHelper.LocalFromJson(modelJson)
  template.Render(local)

let generate (sourceJson: string) =
  ConfigParsing.parseInput sourceJson
  |> JsonResult.map render
  |> JsonResult.toResult
