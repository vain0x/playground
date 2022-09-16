//! # タイマー

// 参考:
//
// - マルチスレッドの処理 (メインスレッドのチャネル):
//      https://github.com/gtk-rs/gtk3-rs/blob/master/examples/multi_threading_context/main.rs
//
// - プログレスバーやタイマーの処理:
//      https://github.com/gtk-rs/gtk3-rs/blob/master/examples/progress_tracker/main.rs

use gtk::{glib, prelude::*, ApplicationWindow, Orientation, WindowPosition};
use std::{
    sync::mpsc,
    thread,
    time::{Duration, SystemTime},
};

enum MainMsg {
    /// タイマーをリセットすることを要求するメッセージ
    ResetTimer {
        #[allow(unused)]
        duration: f64,
    },
    /// アプリの内部状態の変化をUIに反映することを要求するメッセージ
    UpdateUi { fraction: f64, elapsed: f64 },
}

enum WorkerMsg {
    /// スライダーの操作によってdurationが変化したとき
    OnDurationChanged(f64),
    /// リセットボタンが押されて、タイマーの再起動が要求されたとき
    OnResetRequested,
    /// タイマーが発火したとき (一定時間ごとに呼ばれる)
    OnTick,
}

fn build_ui(application: &gtk::Application) {
    // ## メインスレッドとワーカースレッドが相互にメッセージを送るためのチャネルを用意する
    //
    // - tx: 送信用。クローンして複数のスレッドやコールバックに渡せる
    // - rx: 受信用。単一のスレッドに所有される
    let (tx1, rx1) = glib::MainContext::channel(glib::PRIORITY_DEFAULT);
    let (tx2, rx2) = mpsc::sync_channel(0);

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
    // コールバックはワーカーにイベントの発生を意味するメッセージを送る
    // (アプリの状態への参照を発生させないため、それ以外のことはしない)

    slider.connect_value_changed({
        let tx2 = tx2.clone();
        move |slider| {
            eprintln!("slider value changed: {:.1}s", slider.value());
            tx2.send(WorkerMsg::OnDurationChanged(slider.value()))
                .unwrap();
        }
    });

    reset_button.connect_clicked({
        let tx2 = tx2.clone();
        move |_| {
            eprintln!("reset button clicked");
            tx2.send(WorkerMsg::OnResetRequested).unwrap();
        }
    });

    // ## メインスレッド側の挙動を定義する
    //
    // 競合を避けるため、UIオブジェクトへの操作はメインスレッドでのみ行う
    // タイマーはUIオブジェクトの一種とみなす (Windowsの感覚でいえばそう)。すなわち:
    //
    // - タイマーの操作はメインスレッドでのみ行う
    // - タイマーの発火はワーカーへのメッセージの送信だけ行う
    // TODO: 必要なときだけタイマーを動かす

    let update_ui = {
        let progress_bar = progress_bar.clone();
        let elapsed_label = elapsed_label.clone();

        move |fraction: f64, elapsed: f64| {
            progress_bar.set_fraction(fraction);
            elapsed_label.set_text(&format!("{:.1}s", elapsed));
        }
    };

    let _timer = glib::timeout_add_local(Duration::from_millis(100), move || {
        tx2.send(WorkerMsg::OnTick).unwrap();
        Continue(true)
    });

    rx1.attach(None, {
        let update_ui = update_ui.clone();

        move |msg| {
            match msg {
                MainMsg::ResetTimer { duration: _ } => {
                    update_ui(0.0, 0.0);
                }
                MainMsg::UpdateUi { fraction, elapsed } => {
                    update_ui(fraction, elapsed);
                }
            }
            glib::Continue(true)
        }
    });

    // ## ワーカースレッド側の処理を定義する
    //
    // チャネルからメッセージを受け取って、UIオブジェクトのイベントに対する実質的な処理を行う
    // ワーカーは内部状態 (タイマーの長さなど) を単に可変な変数で持つ
    // (メッセージを受け取るたびにループが回るという制御構造なので、可変な状態に並列でアクセスするおそれがない)
    // TODO: アプリの終了時にワーカースレッドを適切に破棄する

    let _worker = {
        let tx1 = tx1.clone();

        thread::spawn(move || {
            let mut current_duration = 10.0;
            let mut started_time = SystemTime::now();

            for msg in rx2 {
                match msg {
                    WorkerMsg::OnDurationChanged(duration) => {
                        current_duration = duration;
                        started_time = SystemTime::now();
                        tx1.send(MainMsg::ResetTimer { duration }).unwrap();
                    }
                    WorkerMsg::OnResetRequested => {
                        let duration = current_duration;
                        started_time = SystemTime::now();
                        tx1.send(MainMsg::ResetTimer { duration }).unwrap();
                    }
                    WorkerMsg::OnTick => {
                        let elapsed_duration = started_time.elapsed().unwrap();
                        let elapsed = elapsed_duration.as_secs_f64().min(current_duration);

                        if elapsed < current_duration + 1e-2 {
                            let fraction = (elapsed / current_duration).min(1.0);
                            tx1.send(MainMsg::UpdateUi { fraction, elapsed }).unwrap();
                        }
                    }
                }
            }
        })
    };

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
