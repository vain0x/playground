.nuget\nuget.exe pack DotNetKit.IO.FileSystem\DotNetKit.IO.FileSystem.csproj.nuspec
.nuget\nuget.exe push *.nupkg -Source https://www.nuget.org/api/v2/package
del /Q *.nupkg
