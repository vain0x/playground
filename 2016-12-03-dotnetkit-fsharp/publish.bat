.nuget\nuget.exe pack DotNetKit.FSharp\DotNetKit.FSharp.fsproj.nuspec -Build
.nuget\nuget.exe pack DotNetKit.ErrorHandling.FSharp\DotNetKit.ErrorHandling.FSharp.fsproj.nuspec -Build
.nuget\nuget.exe push *.nupkg -Source https://www.nuget.org/api/v2/package
del /Q *.nupkg
