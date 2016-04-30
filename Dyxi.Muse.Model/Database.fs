namespace Dyxi.Muse.Model

open Dyxi.Muse.Database

[<AutoOpen>]
module DatabaseExtension =
  let dbx = DbAccess()
  let db = dbx.Entities

  // Login
  let loginUser =
    db.users.Find(1)
