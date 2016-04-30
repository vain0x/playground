namespace Dyxi.Muse.ViewModel

open System
open Dyxi.Muse.Model

[<AutoOpen>]
module Types =
  type LvRow =
    {
      Title       : string
      Composer    : string
      Writer      : string
      Album       : string
      Added       : string
      LastPlayed  : string
      MediaId     : MediaId
    }
