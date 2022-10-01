default: run

.PHONY: build run

build: OptLang/OptLang.fsproj ${wildcard OptLang/*.fs}
	dotnet build -nologo --no-restore

run: build
	dotnet run --project OptLang
