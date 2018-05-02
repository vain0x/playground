namespace Dyxi.Util

open System
open System.Threading
open System.Threading.Tasks

module Async =
  let inject x =
    async { return x }

  let AwaitTaskVoid : (Task -> Async<unit>) =
    Async.AwaitIAsyncResult >> Async.Ignore
