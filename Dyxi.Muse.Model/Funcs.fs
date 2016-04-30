namespace Dyxi.Muse.Model

open System
open System.IO
open Dyxi.Util
open Dyxi.Muse.Database
open Basis.Core
open Chessie.ErrorHandling
open TagLib

module People =
  let name (peopleId: Id) =
    (db.users.Find(peopleId)).name
(*
module Work =
  let addAudioWork name createdOpt composerNames =
    let work =
      db.Works.Create()
      |> tap (fun work ->
          work.Name <- name
          work.Created <- createdOpt
          Database.update ()
          )
    let composers =
      []
      composerNames |> List.map (fun name ->
        db.WorkComposers.Create()
        |> tap (fun composer ->
            composer.WorkId     <- work.Id
            composer.PeopleId   <- People.findOrAdd name
            ))
    in (work, composers)

module MediaPerformer =
  let addTo mediaId names =
    names |> Seq.map (fun name ->
      db.MediaPerformers.Create()
      |> tap (fun performer ->
          performer.MediaId <- mediaId
          performer.Role <- None
          ))

module Media =
  let uploadAudio path workCreatedOpt mediaCreatedOpt =
    let content   = File.ReadAllBytes(path)
    let file      = TagLib.File.Create(path)
    let tag       = file.Tag
    let (work, _) = Work.addAudioWork tag.Title workCreatedOpt (tag.Composers |> Array.toList)
    let media     =
      db.Medias.Create()
      |> tap (fun media ->
          media.Name      <- work.Name
          media.WorkId    <- work.Id
          media.Created   <- mediaCreatedOpt
          media.Extension <- Path.GetExtension(path)
          media.Length    <- file.Length |> uint64
          media.Content   <- content
          )
    let performers =
      []//tag.Performers |> MediaPerformer.addTo media.Id
    in (work, media, performers)

  let tryFindById mediaId =
    let q =
      query {
        for m in db.Medias do
          where (m.Id = mediaId)
          select m
      }
    in q |> Seq.tryHead |> failIfNone "Invalid Media Id"

  let cacheFilePath mediaId mediaExt =
    Path.Combine(loginUser.CacheDir, string mediaId + mediaExt)

  let tryFetchCache mediaId =
    trial {
      let! media    = tryFindById mediaId
      let path      = cacheFilePath mediaId media.Extension
      if File.Exists(path) |> not then
        try
          File.WriteAllBytes (path, media.Content)
        with
        | e -> return! fail (e.Message)
      return path
    }
      //*)

module Coll =
  let fetchMediaList: Coll -> medias [] =
    function
    | MusicColl ->
        dbx.AllMedias() |> Seq.toArray
    | CollId collId ->
        failwith "unimplemented"
