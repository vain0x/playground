//! # タイマー

// 参考:
//
// - メインスレッドのチャネルの利用:
//      https://github.com/gtk-rs/gtk3-rs/blob/master/examples/multi_threading_context/main.rs
//
// - プログレスバーやタイマーの処理:
//      https://github.com/gtk-rs/gtk3-rs/blob/master/examples/progress_tracker/main.rs

use gtk::{glib, prelude::*, ApplicationWindow, Orientation, WindowPosition};
use std::time::{Duration, SystemTime};

enum Msg {
    /// スライダーの操作によってdurationが変化したとき
    OnDurationChanged(f64),
    /// リセットボタンが押されて、タイマーの再起動が要求されたとき
    OnResetRequested,
    /// タイマーが発火したとき (一定時間ごとに呼ばれる)
    OnTick,
}

fn build_ui(application: &gtk::Application) {
    // ## メインスレッドにメッセージを送るチャネルを用意する
    //
    // - tx: 送信用。クローンして複数のコールバックに渡せる
    // - rx: 受信用。単一のコールバックに所有される
    let (tx, rx) = glib::MainContext::channel(glib::PRIORITY_DEFAULT);

    // ## UIを組み立てる
    //
    // この時点ではコールバックを設定しない (所有権でややこしくなるのを避けるため)

    let window = ApplicationWindow::new(application);
    window.set_title("Timer");
    window.set_position(WindowPosition::Center);

    let progress_bar = gtk::ProgressBar::new();
    progress_bar.set_valign(gtk::Align::Center);

    let progress_row = {
        let r = gtk::Box::new(Orientation::Horizontal, 8);
        r.add(&gtk::Label::new(Some("Elapsed Time:")));
        r.add(&progress_bar);
        r
    };

    let elapsed_label = gtk::Label::new(Some("0.0s"));

    let slider = gtk::Scale::with_range(Orientation::Horizontal, 0.0, 100.0, 1.0);
    slider.set_width_request(200);
    slider.set_value(10.0); // 初期値 10s

    let slider_row = {
        let r = gtk::Box::new(Orientation::Horizontal, 4);
        r.add(&gtk::Label::new(Some("Duration:")));
        r.add(&slider);
        r
    };

    let reset_button = gtk::Button::with_label("Reset");

    let column = gtk::Box::new(Orientation::Vertical, 16);
    column.set_margin(24);
    column.add(&progress_row);
    column.add(&elapsed_label);
    column.add(&slider_row);
    column.add(&reset_button);

    window.add(&column);

    // ## UIのイベントにコールバックを接続する
    //
    // コールバックはイベントの発生を意味するメッセージを送る
    // (アプリの状態への参照を発生させないため、それ以外のことはしない)

    slider.connect_value_changed({
        let tx = tx.clone();
        move |slider| {
            eprintln!("slider value changed: {:.1}s", slider.value());
            tx.send(Msg::OnDurationChanged(slider.value())).unwrap();
        }
    });

    reset_button.connect_clicked({
        let tx = tx.clone();
        move |_| {
            eprintln!("reset button clicked");
            tx.send(Msg::OnResetRequested).unwrap();
        }
    });

    // ## イベント発生時の処理を定義する
    //
    // チャネルからメッセージを受け取って、UIオブジェクトのイベントに対する実質的な処理を行う
    //
    // 内部状態 (タイマーの長さなど) は単に可変な変数で持つ
    // (メッセージのハンドラだけが可変な状態にアクセスできるため &mut の一意性が静的に分かる
    //  逆にコールバックごとに処理を書くと共有参照 (&T) が必要になってしまう)

    // TODO: 必要なときだけタイマーを動かす

    let update_ui = {
        let progress_bar = progress_bar.clone();
        let elapsed_label = elapsed_label.clone();

        move |fraction: f64, elapsed: f64| {
            progress_bar.set_fraction(fraction);
            elapsed_label.set_text(&format!("{:.1}s", elapsed));
        }
    };

    let _timer = glib::timeout_add_local(Duration::from_millis(100), {
        let tx = tx.clone();
        move || {
            tx.send(Msg::OnTick).unwrap();
            Continue(true)
        }
    });

    rx.attach(None, {
        let update_ui = update_ui.clone();

        let mut current_duration = 10.0;
        let mut started_time = SystemTime::now();

        move |msg| {
            match msg {
                Msg::OnDurationChanged(duration) => {
                    current_duration = duration;
                    started_time = SystemTime::now();
                    update_ui(0.0, 0.0);
                }
                Msg::OnResetRequested => {
                    started_time = SystemTime::now();
                    update_ui(0.0, 0.0);
                }
                Msg::OnTick => {
                    let elapsed_duration = started_time.elapsed().unwrap();
                    let elapsed = elapsed_duration.as_secs_f64().min(current_duration);

                    if elapsed < current_duration + 1e-2 {
                        let fraction = (elapsed / current_duration).min(1.0);
                        update_ui(fraction, elapsed);
                    }
                }
            }
            glib::Continue(true)
        }
    });

    // ## UIを初期化する

    update_ui(0.0, 0.0);
    window.show_all();
}

fn main() {
    let application = gtk::Application::new(Some("com.example.timer"), Default::default());

    application.connect_activate(|app| {
        build_ui(app);
    });

    application.run();
}
