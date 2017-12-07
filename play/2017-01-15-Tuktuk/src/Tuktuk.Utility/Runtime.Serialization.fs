namespace Tuktuk.Runtime.Serialization

open System.Runtime.Serialization

type ISerializable<'target> =
  abstract Serialize: unit -> 'target
