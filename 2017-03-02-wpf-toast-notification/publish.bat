.nuget\nuget.exe pack DotNetKit.Wpf.ToastNotification\DotNetKit.Wpf.ToastNotification.csproj.nuspec
.nuget\nuget.exe push *.nupkg -Source https://www.nuget.org/api/v2/package
del /Q *.nupkg
