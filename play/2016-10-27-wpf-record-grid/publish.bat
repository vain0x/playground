.paket\paket.exe pack output . version $*
.paket\paket.exe push url https://www.nuget.org file DotNetKit.Wpf.RecordGrid.$*.nupkg
del /Q *.nupkg
