# GTK+ 3

GTK+ 3 のチュートリアルをやってみた。

- 公式サイト: [The GTK Project](https://www.gtk.org/)
- API リファレンス: [GTK+ 3 Reference Manual\: GTK+ 3 Reference Manual](https://developer.gnome.org/gtk3/stable/)

## 概要

GTK+ は GUI アプリケーションを作るためのライブラリ。

コアはC言語で書かれていて、オブジェクト指向設計になっている。バインディングを通じて、他の言語からも利用できる。

## インストール

Ubuntu 18.04 の場合、apt を使って開発用のパッケージをインストールする。

```sh
sudo apt install libgtk-3-dev
```

VSCode 上で glibconfig.h がないというエラーが出る:  [glibconfig.h\: No such file or directory · Issue #6 · dusty-nv/jetson-inference](https://github.com/dusty-nv/jetson-inference/issues/6#issuecomment-280827061)

```sh
sudo cp /usr/lib/x86_64-linux-gnu/glib-2.0/include/glibconfig.h /usr/include/glib-2.0/glibconfig.h
```

## 概要

https://developer.gnome.org/gtk3/stable/gtk.html

GTK+ は以下のライブラリに依存しているらしい

- GLib: データ構造やファイルユーティリティなど。GUI には関係しない
- GObject: 型システムやシグナルシステムなど
- GIO: ファイルシステムを操作するための API など
- cairo: 2D グラフィックライブラリ
- Pango: 多言語の文字列を扱うためのライブラリ (?) PangoLayout オブジェクトは GtkTextview などで文字列を表示するのに使われているらしい
- ATK: アクセシビリティの機能のためのフレームワーク
- GdkPixbuf: 画像を読み込むためのもの
- GDK: ウィンドウシステムとの抽象化層 (X11, Windows など)

GTK+ はボタンなどの基本的なウィジットを提供する。イベントドリブンになっている

##
