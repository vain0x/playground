#include <SDL2/SDL.h>
#include <stdbool.h>
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

	// surface: ピクセルの集まり
	SDL_Surface *surface =
	    SDL_CreateRGBSurface(0, SCREEN_WIDTH, SCREEN_HEIGHT, 32, 0, 0, 0, 0);
	if (!surface) {
		app_abort("SDL_CreateRGBSurface", &subscription);
	}
	subscription_push(&subscription, "FreeSurface", surface,
	                  (void (*)(void *))SDL_FreeSurface);

	// 緑で塗る。
	int status = SDL_FillRect(surface, NULL,
	                          SDL_MapRGB(surface->format, 0x21, 0xfa, 0x21));
	if (status != 0) {
		app_abort("SDL_FillRect", &subscription);
	}

	// texture: ピクセルデータを表すもの (具体的な表現はドライバによる)
	SDL_Texture *texture = SDL_CreateTextureFromSurface(renderer, surface);
	if (!texture) {
		app_abort("SDL_CreateTextureFromSurface", &subscription);
	}
	subscription_push(&subscription, "DestroyTexture", texture,
	                  (void (*)(void *))SDL_DestroyTexture);

	// 描画対象をクリアする。
	status = SDL_RenderClear(renderer);
	if (status != 0) {
		app_abort("SDL_RenderClear", &subscription);
	}

	// テクスチャを描画対象に貼り付ける。(NULL はコピーの範囲を制限しないことを表している。)
	status = SDL_RenderCopy(renderer, texture, NULL, NULL);
	if (status != 0) {
		app_abort("SDL_RenderCopy", &subscription);
	}

	// 描画した内容をウィンドウに反映する。
	SDL_RenderPresent(renderer);

	// // 正しく描画できたか見るために5秒待つ。
	// SDL_Delay(5 * 1000);

	// メインループ
	bool is_running = true;
	while (is_running) {
		// マウスやキーボードなどのイベントを処理する。
		SDL_Event e = {};
		while (SDL_PollEvent(&e)) {
			switch (e.type) {
			case SDL_QUIT: {
				fprintf(stderr, "quitting...\n");
				is_running = false;
				continue;
			}
			case SDL_KEYDOWN: {
				fprintf(stderr, "[TRACE] KEYDOWN key=%d\n", e.key.keysym.sym);
				continue;
			}
			default:
				// ignore
				continue;
			}
		}

		// TODO: 再描画

		// 60 FPS
		SDL_Delay(1000 / 60);
	}

	fprintf(stderr, "OK\n");
	subscription_dispose(&subscription);
	SDL_Quit();
	return 0;
}
