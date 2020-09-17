# Thanks to https://www.willusher.io/sdl2%20tutorials/2013/08/15/lesson-0-linux-command-line

CXX = g++

SDL_LIB = -L/usr/local/lib -lSDL2 -Wl,-rpath=/usr/local/lib
SDL_INCLUDE = -I/usr/local/include

CXXFLAGS = -Wall -c -std=c++17 $(SDL_INCLUDE)
LDFLAGS = $(SDL_LIB)
EXE = app

all: run

run: build
	./$(EXE)

build: $(EXE)

$(EXE): main.o
	$(CXX) $< $(LDFLAGS) -o $@

main.o: src/main.c
	$(CXX) $(CXXFLAGS) $< -o $@

.PHONY: all clean
