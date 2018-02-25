namespace VainZero.LyricsParser

module Parsing =
  open VainZero.LyricsParser.Ast

  let parse (source: string): Result<TrackList, string> =
    let trackList = { Title = ""; Tracks = Array.empty }
    Ok trackList
