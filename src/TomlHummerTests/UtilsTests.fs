module TomlHummer.UtilsTests

open System
open Xunit

[<Fact>]
let timeOfDayPositiveTests() =
  TimeSpan(1, 23, 59, 59)
  |> timeOfDay
  |> is (TimeSpan (0, 23, 59, 59))

[<Fact>]
let timeOfDayNegativeTest () =
  TimeSpan.FromHours(-3.0)
  |> timeOfDay
  |> is (TimeSpan.FromHours(21.0))
