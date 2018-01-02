namespace VainZero.MusicLibrarian

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Net
open System.Reflection
open System.Web
open System.Text
open System.Threading

module Program =
  let dump () =
    async {
      let mediaDirectory = DirectoryInfo(@"/media/owner/OS/repo/media/")
      let outputFile = FileInfo(@"/media/owner/OS/repo/vain0-notes/data/music-library/dump.json")

      let database = FileSystemDatabase.create mediaDirectory

      let! jsonText = DatabaseJsonFormat().ToJson(database)

      use outputStream = outputFile.OpenWrite()
      outputStream.SetLength(0L)
      use writer = new StreamWriter(outputStream)
      do! writer.WriteAsync(jsonText) |> Async.AwaitTask
    }

  [<EntryPoint>]
  let main argv =
    dump () |> Async.RunSynchronously
    0
