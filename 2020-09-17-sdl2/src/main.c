#include <SDL2/SDL.h>
#include <stdio.h>

// typedef const void *nullptr_t;
// const void *nullptr = 0;

const int SCREEN_WIDTH = 640;
const int SCREEN_HEIGHT = 480;

SDL_NORETURN
void sdl_abort(const char *msg) {
	fprintf(stderr, "[ERROR] %s %s\n", msg, SDL_GetError());
	SDL_Quit();
	abort();
}

int main() {
	if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
		sdl_abort("SDL_init");
	}

	const int px = 100;
	const int py = 100;
	SDL_Window *window = SDL_CreateWindow("Hello World!", px, py, SCREEN_WIDTH,
	                                      SCREEN_HEIGHT, SDL_WINDOW_SHOWN);
	if (!window) {
		sdl_abort("SDL_CreateWindow");
	}

	SDL_Renderer *renderer = SDL_CreateRenderer(
	    window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
	if (!renderer) {
		SDL_DestroyWindow(window);
		sdl_abort("SDL_CreateRenderer");
	}

	SDL_Delay(5 * 1000);

	SDL_Quit();
	return 0;
}
