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
    let mediaDirectory = DirectoryInfo(@"/media/owner/OS/repo/media/")
    let outputFile = DirectoryInfo(@"/media/owner/OS/repo/vain0-notes/data/music-library/dump.json")
    let database = FileSystemDatabase.create mediaDirectory
    let jsonText = DatabaseJsonFormat().ToJson(database)
    File.WriteAllText(outputFile.FullName, jsonText)

  [<EntryPoint>]
  let main argv =
    dump ()
    0
