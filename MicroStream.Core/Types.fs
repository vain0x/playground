namespace MicroStream
  open System

  type IAuthenticator =
    abstract AuthenticateAsync: Uri -> Async<option<string>>
