SRCS = src/subscription.c src/main.c

all: run

run: build
	./app

build: $(SRCS)
	gcc -std=c11 -Wall `pkg-config --cflags --libs gtk+-3.0` $(SRCS) -o ./app `pkg-config --libs gtk+-3.0`

.PHONY: all build clean run
