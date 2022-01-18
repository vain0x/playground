default: run

.PHONY: build default dev restore run

dev:
	dotnet watch -- build --nologo --no-restore

run:
	dotnet run -- build

# npm i -g http-server
serve:
	npx http-server ./dist
