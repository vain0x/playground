namespace DotNetKit.FSharp

namespace DotNetKit.FSharp.ErrorHandling
  type Result<'x, 'e> =
    | Ok of 'x
    | Failure of 'e
