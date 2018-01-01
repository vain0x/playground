namespace VainZero.MusicLibrarian

open Expecto

module XspfTests =
  let source = """<?xml version="1.0" encoding="UTF-8"?>
<playlist version="1" xmlns="http://xspf.org/ns/0/">
  <trackList>
    <track>
      <location>../music/album-1/track-1.mp3</location>
      <title>track-1</title>
      <creator>creator-1</creator>
      <album>album-1</album>
    </track>
    <track>
      <location>../music/album-2/track-2.m4a</location>
    </track>
  </trackList>
</playlist>
"""

  [<Tests>]
  let tests =
    testList "parse" [
      testCase "can parse tracks" <| fun _ ->
        let playlistLocation = "playlist-location"
        let playlist = Xspf.parse playlistLocation source
        playlist.Location |> is playlistLocation
        match playlist.Tracks with
        | [|track1; track2|] ->
          track1 |> is
            {
              Location = "../music/album-1/track-1.mp3"
              Title = "track-1"
              Creator = "creator-1"
              Album = "album-1"
            }
          track2 |> is
            {
              Location = "../music/album-2/track-2.m4a"
              Title = ""
              Creator = ""
              Album = ""
            }
        | _ -> Should.never ()
    ]
