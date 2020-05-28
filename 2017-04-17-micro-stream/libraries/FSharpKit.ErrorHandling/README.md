# FSharpKit.ErrorHandling
STATUS: NOT STABLE

## Overview
Provides the Option builder, Result operators/builders, AsyncResult builders.

## Install
### via NuGet
*Coming soon*

### via Paket
There are two ways to install the library.

#### 1. NuGet dependency
*Coming soon*

#### 2. GitHub dependency
In paket.dependencies:

```
github FSharpKit/FSharpKit.ErrorHandling:TAG FSharpKit.ErrorHandling/Option.fs
github FSharpKit/FSharpKit.ErrorHandling:TAG FSharpKit.ErrorHandling/Result.fs
github FSharpKit/FSharpKit.ErrorHandling:TAG FSharpKit.ErrorHandling/AsyncResult.fs
```

where `TAG` is a git tag (e.g. `v1.0.0`).

In paket.references of a project:

```
File: Option.fs FSharpKit.ErrorHandling/
File: Result.fs FSharpKit.ErrorHandling/
File: AsyncResult.fs FSharpKit.ErrorHandling/
```

Of cause, you can omit some of files. See documents of Paket for details.
