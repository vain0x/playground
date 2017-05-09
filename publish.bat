.nuget\nuget.exe pack DotNetKit.Data.Entity.MemoryDatabase\DotNetKit.Data.Entity.MemoryDatabase.csproj.nuspec
.nuget\nuget.exe push *.nupkg -Source https://www.nuget.org/api/v2/package
del /Q *.nupkg
