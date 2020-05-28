namespace MicroStream.Reactive.Linq

open System
open System.Reactive.Disposables
open System.Reactive.Linq

module Observer =
  let performFinally (f: unit -> unit) (observer: IObserver<_>) =
    { new IObserver<_> with
        override this.OnNext(value) =
          observer.OnNext(value)
        override this.OnError(error) =
          try
            observer.OnError(error)
          finally
            f ()
        override this.OnCompleted() =
          try
            observer.OnCompleted()
          finally
            f ()
    }
