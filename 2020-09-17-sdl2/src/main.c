#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "subscription.h"

const char *font_file = "JetBrainsMono-Regular.ttf";

const int SCREEN_WIDTH = 640;
const int SCREEN_HEIGHT = 480;

SDL_NORETURN
void app_abort(const char *msg, Subscription *subscription) {
	fprintf(stderr, "[ERROR] %s\n", msg);

	const char *sdl_error = SDL_GetError();
	if (sdl_error != NULL) {
		fprintf(stderr, "   SDL: %s\n", sdl_error);
	}

	const char *ttf_error = TTF_GetError();
	if (ttf_error != NULL) {
		fprintf(stderr, "   TTF: %s\n", ttf_error);
	}

	subscription_dispose(subscription);
	exit(1);
}

int main() {
	Subscription subscription = subscription_new();

	if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
		app_abort("SDL_Init", &subscription);
	}
	subscription_push0(&subscription, "SDL_Quit", SDL_Quit);

	if (TTF_Init() != 0) {
		app_abort("TTF_Init", &subscription);
	}
	subscription_push0(&subscription, "TTF_Quit", TTF_Quit);

	TTF_Font *font = TTF_OpenFont(font_file, 24);
	if (font == NULL) {
		app_abort("TTF_OpenFont", &subscription);
	}
	subscription_push(&subscription, "TTF_CloseFont", font,
	                  (void (*)(void *))TTF_CloseFont);

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

	// 背景色で塗る。
	SDL_Color background_color = {0x66, 0x21, 0x66};
	int status =
	    SDL_FillRect(surface, NULL,
	                 SDL_MapRGB(surface->format, background_color.r,
	                            background_color.g, background_color.b));
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

	// 文字列を描画する。
	SDL_Color white = {0xfa, 0xfa, 0xfa};
	SDL_Surface *text_surface =
	    TTF_RenderText_Blended(font, "Hello, world!", white);
	if (text_surface == NULL) {
		app_abort("TTF_RenderText_Blended", &subscription);
	}

	SDL_Texture *text_texture =
	    SDL_CreateTextureFromSurface(renderer, text_surface);
	if (text_texture == NULL) {
		app_abort("SDL_CreateTextureFromSurface", &subscription);
	}

	SDL_Rect text_rect = {};
	if (SDL_QueryTexture(text_texture, NULL, NULL, &text_rect.w,
	                     &text_rect.h) != 0) {
		app_abort("SDL_QueryTexture", &subscription);
	}
	fprintf(stderr, "[TRACE] text size = (%d, %d)\n", text_rect.w, text_rect.h);

	text_rect.x = (SCREEN_WIDTH - text_rect.w) / 2;
	text_rect.y = (SCREEN_HEIGHT - text_rect.h) / 2;

	if (SDL_RenderCopy(renderer, text_texture, NULL, &text_rect) != 0) {
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
	return 0;
}
