namespace VainZero.LyricsParser

module Tests =
  open System.IO

  let run sampleDir =
    let mutable errorCount = 0
    let inputs = Directory.GetFiles(sampleDir, "*-lyrics.txt")
    for inputPath in inputs do
      let outputPath = inputPath.Replace("-lyrics.txt", "-lyrics.toml")
      printfn "< %s" inputPath
      printfn "> %s" outputPath
      let source = File.ReadAllText(inputPath)
      match Parsing.parse source with
      | Ok trackList ->
        let toml = Rendering.renderToml trackList
        File.WriteAllText(outputPath, toml)
      | Error message ->
        eprintfn "%s" message
        errorCount <- errorCount + 1
    errorCount
