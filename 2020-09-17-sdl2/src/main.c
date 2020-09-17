#include <SDL2/SDL.h>
#include <stdio.h>
#include <stdlib.h>

#include "subscription.h"

const int SCREEN_WIDTH = 640;
const int SCREEN_HEIGHT = 480;

SDL_NORETURN
void app_abort(const char *msg, Subscription *subscription) {
	fprintf(stderr, "[ERROR] %s %s\n", msg, SDL_GetError());
	subscription_dispose(subscription);
	SDL_Quit();
	abort();
}

int main() {
	Subscription subscription = subscription_new();

	if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
		app_abort("SDL_init", &subscription);
	}

	const int px = 100;
	const int py = 100;
	SDL_Window *window = SDL_CreateWindow("Hello World!", px, py, SCREEN_WIDTH,
	                                      SCREEN_HEIGHT, SDL_WINDOW_SHOWN);
	if (window == NULL) {
		app_abort("SDL_CreateWindow", &subscription);
	}
	subscription_push(&subscription, "DestroyWindow", window,
	                  (void (*)(void *))SDL_DestroyWindow);

	SDL_Renderer *renderer = SDL_CreateRenderer(
	    window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
	if (!renderer) {
		app_abort("SDL_CreateRenderer", &subscription);
	}
	subscription_push(&subscription, "DestroyRenderer", renderer,
	                  (void (*)(void *))SDL_DestroyRenderer);

	SDL_Delay(5 * 1000);

	subscription_dispose(&subscription);
	SDL_Quit();
	return 0;
}
