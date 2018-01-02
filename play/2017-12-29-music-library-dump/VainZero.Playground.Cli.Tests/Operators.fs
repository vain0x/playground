namespace VainZero.MusicLibrarian

open Expecto

module Should =
  let inline be expected actual =
    Expect.equal actual expected "Should be"

  let inline throw<'TException> run =
    Expect.throws run

  let inline never () =
    Expecto.Tests.failtest "Never"

[<AutoOpen>]
module TestOperators =
  let inline is expected actual =
    Should.be expected actual
