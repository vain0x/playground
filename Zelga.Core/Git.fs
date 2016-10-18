namespace Zelga.Core

open Zelga.Core.Utility

module Git =
  let timeout = 30000

  let tryExecute command =
    Diagnostics.tryExecute timeout ("git", command)

  type User =
    {
      Name                      : string
      Email                     : string
    }

  let tryGetCurrentUser () =
    tryExecute "config --get user.name" |> Option.bind
      (fun name ->
        tryExecute "config --get user.email" |> Option.map
          (fun email ->
            let name = name |> String.trim
            let email = email |> String.trim
            if (name |> String.isNullOrEmpty) || (email |> String.isNullOrEmpty) then
              failwithf "Your user name or email address for current Git repository is unset. (Git リポジトリーにユーザー名またはメールアドレスが正しく設定されていません。)" name email
            else
              {
                Name                  = name
                Email                 = email
              }
          )
      )
