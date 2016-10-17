namespace Zelga.Core

type User =
  {
    Name                : string
    Email               : string
  }
with
  static member Create(name, email) =
    {
      Name              = name
      Email             = email
    }

module LoginInfo =
  let mutable Current =
    User.Create("debugger", "debugger@example.com")
