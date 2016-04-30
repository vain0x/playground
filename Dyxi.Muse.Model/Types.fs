namespace Dyxi.Muse.Model

[<AutoOpen>]
module Types =
  type Id = int
  type MediaId = Id

  type Coll =
    | MusicColl
    | CollId        of Id
