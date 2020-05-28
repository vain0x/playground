namespace AdhocFs

open System.IO
open FsYaml
open FsYaml.NativeTypes
open FsYaml.CustomTypeDefinition

/// A storage which saves a value by serializing it into YAML.
type YamlFileStorage<'x>(_file: FileInfo) =
  interface IStorage<'x> with
    member this.Load() =
      let text = File.ReadAllText(_file.FullName)
      in text |> Yaml.loadWith<'x> [StorableAsYaml<'x>.Definition]

    member this.Save(value) =
      let text = value |> Yaml.dumpWith<'x> [StorableAsYaml<'x>.Definition]
      in File.WriteAllText(_file.FullName, text)

    member this.Delete() =
      if _file.Exists then
        _file.Delete()

and StorableAsYaml<'x>(_file: FileInfo) =
  inherit Storable<'x>(YamlFileStorage<'x>(_file))

  member this.File = _file

  static member val Definition: TypeDefinition =
    {
      Accept =
        isGenericTypeDef typedefof<StorableAsYaml<_>>
      Construct =
        fun construct' t yaml ->
          let path = construct' typedefof<string> yaml :?> string
          in new StorableAsYaml<'x>(FileInfo(path)) :> obj
      Represent =
        fun represent' t value ->
          let value = value :?> StorableAsYaml<'x>
          in represent' typedefof<string> ((value.File: FileInfo).FullName)
    }
