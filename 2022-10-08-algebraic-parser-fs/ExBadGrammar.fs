module internal ExBadGrammar

module P = ParserV2.ParserCombinator

let private newGrammar () : P.Grammar<unit, _> =
  // expects an ambiguity warning: rhs mustn't nullable
  let r1 = P.leftRec (P.eps ()) (P.eps ()) (fun _ _ -> ())

  // it should detect direct left-recursion
  let r2 =
    P.mu "DirectLeftRec" (fun local -> P.opt (P.rule2 local (P.expect 1 ignore) (fun _ _ -> ())) ignore)

  // it should detect indirect left-recursion
  // mu X. (mu Y. X?) '2'
  // (same as '2'+)
  let r3 =
    P.mu "IndirectLeftRec" (fun local ->
      let inner = P.mu "Inner" (fun _ -> P.opt local ignore)

      P.opt (P.rule2 inner (P.expect 2 ignore) (fun _ _ -> ())) ignore)

  P.build (P.choice [ r1; r2; r3 ]) []

let internal tests () =
  let grammar = newGrammar ()
  P.parseV2 (fun _ -> 0) Array.empty grammar
