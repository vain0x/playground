//! # Circle Drawer

// [Custom Drawing: GTK+ 3 Reference Manual](https://developer-old.gnome.org/gtk3/stable/ch01s05.html)
// [Basic drawing in PyCairo](https://zetcode.com/gfx/pycairo/basicdrawing/)
// [Gtk - 3.0: The GTK Input and Event Handling Model](https://docs.gtk.org/gtk3/input-handling.html)

use gtk::{gdk, glib, prelude::*, ApplicationWindow, Orientation, WindowPosition};
use std::{
    f64::consts::PI,
    sync::{Arc, Mutex},
};

#[derive(Debug)]
enum Msg {
    OnInit,
    OnUndoClick,
    OnRedoClick,
    OnCanvasClick(f64, f64),
}

fn build_ui(application: &gtk::Application) {
    let (tx, rx) = glib::MainContext::channel(glib::PRIORITY_DEFAULT);

    // ## UIを組み立てる

    let window = ApplicationWindow::new(application);
    window.set_title("Circle Drawer");
    window.set_position(WindowPosition::Center);

    let column = gtk::Box::new(Orientation::Vertical, 16);
    column.set_margin(24);
    column.set_hexpand(true);
    column.set_vexpand(true);

    let undo_button = gtk::Button::with_label("Undo");
    let redo_button = gtk::Button::with_label("Redo");

    {
        let row = gtk::Box::new(Orientation::Horizontal, 16);
        row.set_halign(gtk::Align::Center);
        row.add(&undo_button);
        row.add(&redo_button);
        column.add(&row);
    }

    let canvas = gtk::DrawingArea::new();
    {
        canvas.set_size_request(300, 300);
        let frame = gtk::Frame::new(None);
        frame.add(&canvas);
        column.add(&frame);
    }

    window.add(&column);

    // ## UIのイベントにコールバックを接続する

    undo_button.connect_clicked({
        let tx = tx.clone();
        move |_| tx.send(Msg::OnUndoClick).unwrap()
    });
    redo_button.connect_clicked({
        let tx = tx.clone();
        move |_| tx.send(Msg::OnRedoClick).unwrap()
    });
    canvas.connect_button_press_event({
        let tx = tx.clone();
        move |_, ev| {
            let (x, y) = ev.position();
            tx.send(Msg::OnCanvasClick(x, y)).unwrap();
            Inhibit(false)
        }
    });

    let circles = Arc::new(Mutex::new(vec![]));

    // drawの処理はここで同期的に行う必要があり、メッセージを送るだけにはできない
    canvas.connect_draw({
        let circles = Arc::clone(&circles);

        move |canvas, ctx| {
            // マウスポインタの位置を取得する
            // (canvasの左上隅を原点する座標系で取得される)
            let (mx, my) = (|| -> Option<(f64, f64)> {
                let window = canvas.window()?;
                let display = gdk::Display::default()?;
                let device_manager = display.device_manager()?;
                let mouse = device_manager.client_pointer()?;

                let (_, x, y, _) = window.device_position(&mouse);
                Some((x as f64, y as f64))
            })()
            .unwrap();

            let circles = circles.lock().unwrap();
            let circles = circles.as_slice();

            let hit = circles
                .iter()
                .enumerate()
                .rev()
                .find_map(|(i, (cx, cy, r))| {
                    if f64::hypot(mx - cx, my - cy) < r + 1e-6 {
                        Some(i)
                    } else {
                        None
                    }
                })
                .unwrap_or(usize::MAX);

            ctx.set_line_width(1.0);

            for (i, &(cx, cy, r)) in circles.iter().enumerate() {
                if i == hit {
                    ctx.set_source_rgb(0.3, 0.3, 0.3);
                    ctx.arc(cx, cy, r, 0.0, PI * 2.0);
                    ctx.stroke().unwrap();

                    ctx.set_source_rgb(0.6, 0.6, 0.6);
                    ctx.arc(cx, cy, r, 0.0, PI * 2.0);
                    ctx.fill().unwrap();
                    continue;
                }

                ctx.set_source_rgb(0.3, 0.3, 0.3);
                ctx.arc(cx, cy, r, 0.0, PI * 2.0);
                ctx.stroke().unwrap();

                ctx.set_source_rgb(1.0, 1.0, 1.0);
                ctx.arc(cx, cy, r, 0.0, PI * 2.0);
                ctx.fill().unwrap();
            }
            Inhibit(false)
        }
    });

    canvas.connect_motion_notify_event(move |canvas, _| {
        canvas.queue_draw();
        Inhibit(false)
    });

    // クリックやマウス移動によるイベントが発生するようにする
    canvas.set_events(gdk::EventMask::BUTTON_PRESS_MASK | gdk::EventMask::POINTER_MOTION_MASK);

    // ## イベントを処理する

    rx.attach(None, {
        let _tx = tx.clone();
        let mut redo = vec![];

        move |msg| {
            eprintln!("msg {:?}", msg);

            match msg {
                Msg::OnInit => {
                    //
                }
                Msg::OnUndoClick => {
                    if let Some(circle) = circles.lock().unwrap().pop() {
                        redo.push(circle);
                        canvas.queue_draw();
                    }
                }
                Msg::OnRedoClick => {
                    if let Some(circle) = redo.pop() {
                        circles.lock().unwrap().push(circle);
                        canvas.queue_draw();
                    }
                }
                Msg::OnCanvasClick(x, y) => {
                    redo.clear();
                    circles.lock().unwrap().push((x, y, 20.0));
                    canvas.queue_draw();
                }
            }

            undo_button.set_sensitive(!circles.lock().unwrap().is_empty());
            redo_button.set_sensitive(!redo.is_empty());
            Continue(true)
        }
    });

    // ## UIを初期化する

    tx.send(Msg::OnInit).unwrap();
    window.show_all();
}

fn main() {
    let application = gtk::Application::new(Some("com.example.circle-drawer"), Default::default());

    application.connect_activate(|app| {
        build_ui(app);
    });

    application.run();
}
