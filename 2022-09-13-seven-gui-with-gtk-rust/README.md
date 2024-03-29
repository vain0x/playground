# 7 GUIs with Gtk + Rust

7種類のGUIアプリケーションのサンプルを作る試み

参考: [7GUIs](https://7guis.github.io/7guis/)

- [x] Counter
- [x] Temperature Converter
- [x] Flight Booker
- [x] Timer
- [x] CRUD
- [x] Circle Drawer
- [x] Cells

## 動かしかた

- Rustをインストールする
- [Gtk3](https://www.gtk.org/docs/installations/) をインストールする

```sh
# 実行
cargo run --bin counter
```

### テスト

```sh
cargo test --bin cells
```

(テストがあるのはCellsのみ)

## リンク

- [The GTK Project](https://www.gtk.org/)
    Gtk 3.x の公式サイト (英語)
- [Gtk - 3.0: Getting Started with GTK](https://docs.gtk.org/gtk3/getting_started.html)
    公式のチュートリアル (C言語、英語)
- [GTK 3 Rust bindings](https://gtk-rs.org/gtk3-rs/):
    Rustバインディング (RustからGtkを使うためのライブラリ) であるgtk-rsの公式サイト
- <https://github.com/RainMark/gtk3-tutorial>
    Gtk3の (非公式の？) チュートリアル。
    ライブラリを使うには基本的な流れを押さえる必要がある

## スクリーンショット

![counter](docs/screenshots/counter.png)
![temperature-converter](docs/screenshots/temperature-converter.png)
![flight-booker](docs/screenshots/flight-booker.png)
![timer](docs/screenshots/timer.png)
![crud](docs/screenshots/crud.png)
![circle-drawer](docs/screenshots/circle-drawer.png)
![cells](./docs/screenshots/cells.png)
