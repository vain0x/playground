default: test.timestamp

.phony: build default test

build: build.timestamp

build.timestamp: \
	pcc.sln \
	${wildcard */*.fs} \
	${wildcard */*.fsproj}
	dotnet build -nologo && touch build.timestamp

test: test.timestamp

test.timestamp: build.timestamp \
	${wildcard tests/syntax/*.simple}
	dotnet run --project Pcc -nologo --no-build ${wildcard tests/syntax/*.simple} && touch test.timestamp
