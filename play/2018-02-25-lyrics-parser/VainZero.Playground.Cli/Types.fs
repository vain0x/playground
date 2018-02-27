namespace VainZero.LyricsParser.Ast

  type Metadata =
    {
      Title: string
      Writers: string[]
      Composers: string[]
      Performers: string[]
      TrackNumber: option<int>
      ReleaseYear: option<int>
      Note: string
      Tie: string[]
      Entries: Map<string, string[]>
    }

  type Song =
    {
      Metadata: Metadata
      Lyrics: string
    }

  type Track =
    | SongTrack
      of Song
    | AlbumTrack
      of Metadata * Song[]

  type TrackList =
    {
      Tracks: Track[]
    }
