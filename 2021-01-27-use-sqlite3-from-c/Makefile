.PHONY: build clean download run

default: build

# ------------------------------------------------
# Build
# ------------------------------------------------

build: target/app

target/app: main.c vendor/sqlite3/sqlite3.o
	./build



# Build SQLite3

vendor/sqlite3/sqlite3.o: vendor/sqlite3/sqlite3.c
	./build-sqlite3

vendor/sqlite3/sqlite3.c:
	./download

# ------------------------------------------------
# Run
# ------------------------------------------------

run: target/app
	./run

# ------------------------------------------------
# Clean
# ------------------------------------------------

clean:
	./clean
