module private Program

[<EntryPoint>]
let main _ =
  ExArith.tests ()
  ExBadGrammar.tests ()
  ExMiniLang.tests ()
  ExNumberSequence.tests ()
  0
