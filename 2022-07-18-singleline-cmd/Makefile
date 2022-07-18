# For linux
# Require gcc

default: build

.PHONY: build test

target/singleline: singleline/on_linux.c
	mkdir -p target
	gcc -std=c17 -Wall -O1 -g singleline/on_linux.c -o target/singleline

build: target/singleline

test: target/singleline
	cat input.txt | xargs -I{} sh -c 'echo {}; sleep 1s' | target/singleline
