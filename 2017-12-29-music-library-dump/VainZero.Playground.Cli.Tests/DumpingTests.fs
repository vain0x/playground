namespace VainZero.MusicLibrarian

open System
open Expecto
open VainZero.MusicLibrarian.JsonSerialization

module DumpingTests =
  let expected = """{
  "music_metadatas": [
    {
      "location": "../music/album-1/01 1st track.mp3",
      "title": "1st track",
      "performers": [
        "performer-1",
        "performer-2"
      ],
      "composers": [
        "composer-1"
      ],
      "album": "album-1",
      "track_number": 1,
      "release_year": 2018
    },
    {
      "location": "../music/void/void track.m4a",
      "title": "void track",
      "performers": [

      ],
      "composers": [

      ]
    }
  ],
  "playlists": [

  ]
}"""

  let databaseJsonFormat = DatabaseJsonFormat()

  [<Tests>]
  let tests =
    testList "dump" [
      testCase "can dump" <| fun _ ->
        let musicMetadataDto title performers composers album trackNumber releaseYear filePath =
          {
            Title = title
            Performers = performers
            Composers = composers
            Album = album
            TrackNumber = trackNumber
            ReleaseYear = releaseYear
            Location = filePath
          }: MusicMetadataDto
        let musicMetadatas: MusicMetadataDto[] =
          [|
            {
              Title = "1st track"
              Performers = [|"performer-1"; "performer-2"|]
              Composers = [|"composer-1"|]
              Album = Some "album-1"
              TrackNumber = Some 1
              ReleaseYear = Some 2018
              Location = "../music/album-1/01 1st track.mp3"
            }
            {
              Title = "void track"
              Performers = [||]
              Composers = [||]
              Album = None
              TrackNumber = None
              ReleaseYear = None
              Location = "../music/void/void track.m4a"
            }
          |]
        let databaseDto: DatabaseDto =
          {
            MusicMetadatas = musicMetadatas
            Playlists = [||]
          }
        databaseJsonFormat.DtoToJson(databaseDto) |> is expected
    ]
