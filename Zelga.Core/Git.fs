namespace Zelga.Core

open Zelga.Core.Utility

module Git =
  let execute command =
    let timeout = 30000
    Diagnostics.execCmdline timeout "git"
