//! # Circle Drawer

// [Custom Drawing: GTK+ 3 Reference Manual](https://developer-old.gnome.org/gtk3/stable/ch01s05.html)

use gtk::{gdk, glib, prelude::*, ApplicationWindow, Orientation, WindowPosition};

#[derive(Debug)]
enum Msg {
    OnInit,
    OnUndoClick,
    OnRedoClick,
    OnClick(f64, f64),
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
    canvas.connect_draw(|_canvas, _dx| {
        // TODO: DRAW
        Inhibit(false)
    });
    canvas.connect_button_press_event({
        let tx = tx.clone();
        move |_, ev| {
            let (x, y) = ev.position();
            tx.send(Msg::OnClick(x, y)).unwrap();
            Inhibit(false)
        }
    });
    canvas.set_events(gdk::EventMask::BUTTON_PRESS_MASK);

    // ## イベントを処理する

    rx.attach(None, {
        let _tx = tx.clone();

        move |msg| {
            eprintln!("msg {:?}", msg);

            match msg {
                Msg::OnInit => {
                    //
                }
                Msg::OnUndoClick => {
                    // TODO
                }
                Msg::OnRedoClick => {
                    // TODO
                }
                Msg::OnClick(_x, _y) => {
                    // TODO
                }
            }
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
