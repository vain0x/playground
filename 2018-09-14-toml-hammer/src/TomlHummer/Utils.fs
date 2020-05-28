namespace rec TomlHummer
  open System

  [<assembly: System.Runtime.CompilerServices.InternalsVisibleTo("TomlHummerTests")>]
  do ()

  [<AutoOpen>]
  module Operators =
    [<Literal>]
    let Day = TimeSpan.TicksPerDay

    let internal timeOfDay (t: TimeSpan) =
      TimeSpan.FromTicks((t.Ticks % Day + Day) % Day)
