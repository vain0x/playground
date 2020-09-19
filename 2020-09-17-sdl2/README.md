
- SDL の公式サイト: [Simple DirectMedia Layer - Homepage](https://www.libsdl.org/)
    - チュートリアル: [TwinklebearDev SDL 2.0 Tutorial Index](https://www.willusher.io/pages/sdl2/)
- GCC の環境変数に関して: [Environment Variables (Using the GNU Compiler Collection (GCC))](https://gcc.gnu.org/onlinedocs/gcc/Environment-Variables.html)

```sh
# SDL2 のインストール
# チュートリアルを参照
```

```sh
# SDL_ttf のインストール

FREETYPE2_URL="https://download.savannah.gnu.org/releases/freetype/freetype-2.10.0.tar.gz"
SDL_TTF_URL="https://www.libsdl.org/projects/SDL_ttf/release/SDL2_ttf-2.0.15.tar.gz"

(
    curl -sL "$FREETYPE2_URL" | tar -xz -C /tmp
    cd /tmp/freetype-2.10.0
    ./configure
    make
    sudo make install
)

(
    curl -sL "$SDL_TTF_URL" | tar -xz -C /tmp
    cd /tmp/SDL2_ttf-2.0.15
    ./configure
    make
    sudo make install
)
```

```sh
# サンプルで使うフォントファイルのダウンロード (なんでもいいけど)

curl -sL "https://github.com/JetBrains/JetBrainsMono/raw/master/ttf/JetBrainsMono-Regular.ttf" -o "JetBrainsMono-Regular.ttf"
```
