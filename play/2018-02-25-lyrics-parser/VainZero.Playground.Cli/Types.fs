namespace VainZero.LyricsParser.Ast

  type Metadata =
    {
      Writers: string[]
      Composers: string[]
      Performers: string[]
      TrackNumber: option<int>
      ReleaseYear: option<int>
      Notes: option<string>
      Tie: string[]
    }

  type Song =
    {
      Metadata: Metadata
      Lyrics: string
    }

  type Track =
    | Song
      of Song
    | Album
      of Metadata * Song[]

  type TrackList =
    {
      Title: string
      Tracks: Track[]
    }
