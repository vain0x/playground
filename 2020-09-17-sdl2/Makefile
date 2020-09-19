# Thanks to https://www.willusher.io/sdl2%20tutorials/2013/08/15/lesson-0-linux-command-line

CXX = g++

SDL_LIB = -L/usr/local/lib -lSDL2 -lSDL2_ttf -Wl,-rpath=/usr/local/lib
SDL_INCLUDE = -I/usr/local/include

OBJS = subscription.o main.o
SRCS = src/subscription.c src/main.c
CXXFLAGS = -Wall -c -std=c++17 $(SDL_INCLUDE)
LDFLAGS = $(SDL_LIB)
EXE = app

all: run

run: build
	./$(EXE)

build: $(SRCS)
	$(CXX) $(CXXFLAGS) $(SRCS)
	$(CXX) -Wl,--start-group $(OBJS) -Wl,--end-group $(LDFLAGS) -o $(EXE)

.PHONY: all clean
