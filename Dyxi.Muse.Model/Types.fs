namespace Dyxi.Muse.Model

[<AutoOpen>]
module Types =
  type Id = uint32
  type MediaId = Id

  type Coll =
    | MusicColl
    | CollId        of Id
