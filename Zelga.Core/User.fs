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
