namespace VainZero.MusicLibrarian
  open System

  // Relative path to the Media directory.
  type RelativeFilePath = string

  type MusicMetadata =
    {
      Title: string
      Performers: string[]
      Composers: string[]
      Album: option<string>
      TrackNumber: option<int>
      ReleaseYear: option<int>
      FilePath: RelativeFilePath
    }

  type MusicTrack =
    {
      Location: RelativeFilePath
      Title: option<string>
      Creator: option<string>
      Album: option<string>
    }

  type Playlist =
    {
      Location: RelativeFilePath
      Tracks: MusicTrack[]
    }

  [<AbstractClass>]
  type MusicRepository() =
    abstract FindAll: unit -> MusicMetadata[]
    abstract TryFindByPath: RelativeFilePath -> option<MusicMetadata>

  [<AbstractClass>]
  type PlaylistRepository() =
    abstract FindAll: unit -> Playlist[]
    abstract TryFindByPath: RelativeFilePath -> option<Playlist>

  type Database =
    {
      MusicRepository: MusicRepository
      PlaylistRepository: PlaylistRepository
    }

namespace VainZero.MusicLibrarian.JsonSerialization
  type MusicMetadataDto =
    {
      Location: string
      Title: string
      Performers: string[]
      Composers: string[]
      Album: option<string>
      TrackNumber: option<int>
      ReleaseYear: option<int>
    }

  type MusicTrackDto =
    {
      Location: string
      Title: option<string>
      Creator: option<string>
      Album: option<string>
    }

  type PlaylistDto =
    {
      Location: string
      Tracks: MusicTrackDto[]
    }

  type DatabaseDto =
    {
      MusicMetadatas: MusicMetadataDto[]
      Playlists: PlaylistDto[]
    }
