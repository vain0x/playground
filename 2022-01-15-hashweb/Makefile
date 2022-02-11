default: run

.PHONY: build default dev restore run

dev:
	dotnet watch -- run -- build

run:
	dotnet run -- build

# npm i -g http-server
serve:
	npx http-server ./dist
