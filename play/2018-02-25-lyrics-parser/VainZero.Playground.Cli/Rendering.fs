namespace VainZero.LyricsParser

module Rendering =
  open VainZero.LyricsParser.Ast

  let renderToml (trackList: TrackList): string =
    let output =
      sprintf """
[trackList]
title = "%s"
"""
        trackList.Title
    output.TrimStart()
