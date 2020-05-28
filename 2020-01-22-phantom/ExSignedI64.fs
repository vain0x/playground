module ExSignedI64

open Core

let zeroPhase =
  phaseOfCase "Zero" (KSelf, IntPat 0)

let positivePhase =
  phaseOfCase "Positive" (KSelf, RangePat (Some (IntPat 1), None))

let negativePhase =
  phaseOfCase "Negative" (KSelf, RangePat (None, Some (IntPat 0)))

let signedI64Phantom =
  {
    PhantomName = "SignedI64"
    Phases = [
      zeroPhase
      positivePhase
      negativePhase
    ]
  }

let log2Fn =
  {
    FnName = "log2"
    Params = [
      localOf "x" (PhaseTy positivePhase)
    ]
    Result = localOf "result" I64Ty
    ResultTy = I64Ty
    Ensures = []
    Body = KReturn (KInt 0)
  }

let paramCheckFn =
  let a = localOf "a" I64Ty
  let discard = localOf "discard" I64Ty

  {
    FnName = "paramCheck"
    Params = [a]
    Result = localOf "result" I64Ty
    ResultTy = I64Ty
    Ensures = []
    Body =
      // log2(8)
      KCall (
        log2Fn, [KInt 8],
        discard,

        KCase (
          // a @ 1..
          KName a,
          RangePat (Some (IntPat 1), None),

          // then log2(a)
          KCall (
            log2Fn,
            [KName a],
            discard,
            KReturn (KInt 0)
          ),

          // else
          KReturn (KInt 0)
      ))
  }

let isPositiveFn =
  let x = localOf "x" I64Ty
  let result = localOf "y" I64Ty

  {
    FnName = "isPositive"
    Params = [
      x
    ]
    Result = result
    ResultTy = I64Ty
    Ensures = [
      CaseConstr (KName result, IntPat 1),
        CaseConstr (KName x, TyPat (PhaseTy positivePhase))
    ]
    Body =
      KCase (
        // a @ 1..
        KName x,
        RangePat (Some (IntPat 1), None),

        // then
        KReturn (KInt 1),

        // else
        KReturn (KInt 0)
      )
  }

let useEnsuresFn =
  let a = localOf "a" I64Ty
  let b = localOf "b" I64Ty
  let discard = localOf "discard" I64Ty

  {
    FnName = "useEnsures"
    Params = [a]
    ResultTy = I64Ty
    Result = localOf "result" I64Ty
    Ensures = []
    Body =
      // isPositive(a)
      KCall (
        isPositiveFn, [KName a],
        b,

        KCase (
          // b = 1 (isPositive(a) が真？)
          KName b,
          IntPat 1,

          // then log2(a)
          KCall (
            log2Fn,
            [KName a],
            discard,
            KReturn (KInt 0)
          ),

          // else
          KReturn (KInt 0)
      ))
  }
