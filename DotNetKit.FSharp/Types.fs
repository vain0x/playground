namespace DotNetKit.FSharp

namespace DotNetKit.FSharp.ErrorHandling
  type Result<'x, 'e> =
    | Success of 'x
    | Failure of 'e
