namespace AdhocFs

open System

type IStorage<'x> =
  abstract member Load: unit -> 'x
  abstract member Save: 'x -> unit
  abstract member Delete: unit -> unit

type Storable<'x>(_storage: IStorage<'x>) =
  let _lockObj = new obj()
  let mutable _valueOpt = (None: option<'x>)

  let _get () =
    match _valueOpt with
    | Some x -> x
    | None ->
      lock _lockObj (fun () ->
        let x = _storage.Load()
        _valueOpt <- Some x
        x
        )

  let _store () =
    _valueOpt |> Option.iter (fun value ->
      lock _lockObj (fun () ->
        _storage.Save(value)
        _valueOpt <- None
        ))

  member this.IsLoaded =
    _valueOpt |> Option.isSome

  member this.Value
    with get () =
      _get ()
    and set v =
      _valueOpt <- Some v

  member this.TryGet() =
    _get ()

  member this.Fetch() =
    _get () |> ignore

  member this.Store() =
    _store () |> ignore

  member this.Clear() =
    _storage.Delete()
    _valueOpt <- None

  override this.Finalize() =
    this.Store()

  interface IDisposable with
    member this.Dispose() =
      this.Store()
