namespace DotNetKit.FSharp

namespace DotNetKit.FSharp.ErrorHandling
  type Result<'x, 'e> =
    | Ok of 'x
    | Error of 'e
