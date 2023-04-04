module Program

open Rx

[<EntryPoint>]
let main _ =
  let basicGlitch () =

    rx {
      let x = Var(true)
      let y = cache { return not +x }
      // this should always be false
      let res = cache { return +x && +y }
      let mock = mockOb res

      // Initially res is false
      assert (not res.Current)

      // Setting x to false, res holds false
      x <+ false
      assert (not res.Current)

      // Switching x to true again, res still holds false
      x <+ true
      assert (not res.Current)

      // mock.currents.asserts()
      // mock.messages.asserts()
    }

  basicGlitch ()
  0
