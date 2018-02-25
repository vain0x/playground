namespace VainZero.LyricsParser

module Tests =
  open System.IO

  let run sampleDir =
    let mutable errorCount = 0
    let inputs = Directory.GetFiles(sampleDir, "*-lyrics.txt")
    for inputFilePath in inputs do
      let outputPath = inputFilePath.Replace("-lyrics.txt", "-lyrics.toml")
      let source = File.ReadAllText(inputFilePath)
      match Parsing.parse source with
      | Ok trackList ->
        let toml = Rendering.renderToml trackList
        File.WriteAllText(outputPath, toml)
      | Error message ->
        eprintfn "%s" message
        errorCount <- errorCount + 1
    errorCount
