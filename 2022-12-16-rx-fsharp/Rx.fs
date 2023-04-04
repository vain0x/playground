module rec Rx

let inline private todo () = failwith "todo"

// scope(fun rx -> makeVm(rx.newSignal(() -> f())))
//

// events: raise, observe, merge, map, flatten
// reactor (next, loopUntil)
// signal (now, apply, changes)
// dataFlowReactive (emit, switchTo, delay, next)

// Var -> SourceSignal(abstract) -> SourceReactive(abstract) -> Publisher(trait) -> MessageCachingReactive(trait) -> Dependency(trait)

// DataFlowBase: (body, next, loopUntil, delay, isAlive, dispose, mayAbort, continueNow, continueLater)

// Reactive = (_value, subscribe, current, now, message, msg, toSignal, toEvents, )
// Dependency = Reactive + DependencyNode



module MessageCachingReactive =
  // inherit Dependency
  // message(dep)
  // cacheMessage(msg)

// trait in Scala
type Publisher<'T> =
  interface end

type PublisherState =
  { mutable Dependents: Dependents }

module PublisherTrait =
  let subscribe publisherState (dep: Dependent) =
   ()

  let clearDependents state =
    let old = state.Dependents
    state.Dependents <- newDEpendents
    old

  let propagate state msg =
    cacheMessage msg
    Engine.propagateFrom this

[<Abstract>]
type SourceReactive<'T>() =
  extends Publisher<'T>

[<Abstract>]
type SourceSignal<'T>() =
  inherit SourceReactive<'T>()

  member this.Emit() =
    this.EmitWith(this.Current)

  // this.Cached = this

type Var<'T>(init: 'T) =
  inherit SourceSignal<'T>()

  let mutable current = init

  member this.Update(newValue: 'T) =
    if init <> newValue then
      current <- newValue
      this.Emit()

let (~+) (r: Var) = r.Load()
