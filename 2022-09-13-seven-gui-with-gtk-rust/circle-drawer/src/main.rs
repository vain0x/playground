//! # Circle Drawer

// [Custom Drawing: GTK+ 3 Reference Manual](https://developer-old.gnome.org/gtk3/stable/ch01s05.html)
// [Basic drawing in PyCairo](https://zetcode.com/gfx/pycairo/basicdrawing/)
// [Gtk - 3.0: The GTK Input and Event Handling Model](https://docs.gtk.org/gtk3/input-handling.html)

use gtk::{
    gdk::{
        self,
        ffi::{GDK_BUTTON_PRIMARY, GDK_BUTTON_SECONDARY},
    },
    glib,
    prelude::*,
    ApplicationWindow, Orientation, WindowPosition,
};
use std::{
    f64::consts::PI,
    sync::{Arc, Mutex},
};

#[derive(Debug)]
enum Msg {
    OnInit,
    OnUndoClick,
    OnRedoClick,
    OnCanvasLeftClick(f64, f64),
    OnCanvasRightClick(f64, f64),
    OnAdjustScaleChange { radius: f64 },
    OnAdjustCancel,
    OnAdjustOk,
}

enum Action {
    Add {
        position: (f64, f64),
        radius: f64,
    },
    Remove {
        position: (f64, f64),
        radius: f64,
    },
    Resize {
        index: usize,
        old_radius: f64,
        new_radius: f64,
    },
}

impl Action {
    fn inverse(self) -> Action {
        match self {
            Action::Add { position, radius } => Action::Remove { position, radius },
            Action::Remove { position, radius } => Action::Add { position, radius },
            Action::Resize {
                index,
                old_radius,
                new_radius,
            } => Action::Resize {
                index,
                old_radius: new_radius,
                new_radius: old_radius,
            },
        }
    }
}

fn build_ui(application: &gtk::Application) {
    let (tx, rx) = glib::MainContext::channel(glib::PRIORITY_DEFAULT);

    // ## UIを組み立てる

    let window = ApplicationWindow::new(application);
    window.set_title("Circle Drawer");
    window.set_position(WindowPosition::Center);

    let window_column = gtk::Box::new(Orientation::Vertical, 16);
    window_column.set_margin(24);
    window_column.set_hexpand(true);
    window_column.set_vexpand(true);

    let undo_button = gtk::Button::with_label("Undo");
    let redo_button = gtk::Button::with_label("Redo");

    {
        let row = gtk::Box::new(Orientation::Horizontal, 16);
        row.set_halign(gtk::Align::Center);
        row.add(&undo_button);
        row.add(&redo_button);
        window_column.add(&row);
    }

    let canvas = gtk::DrawingArea::new();
    {
        canvas.set_size_request(300, 300);
        let frame = gtk::Frame::new(None);
        frame.add(&canvas);
        window_column.add(&frame);
    }

    window.add(&window_column);

    // or gtk::Dialog
    let dialog = gtk::Window::new(gtk::WindowType::Toplevel);
    dialog.set_title("Adjust");
    dialog.set_transient_for(Some(&window));
    dialog.set_position(WindowPosition::CenterOnParent);
    dialog.set_modal(true);

    let scale = gtk::Scale::with_range(Orientation::Horizontal, 1.0, 30.0, 0.5);
    scale.set_width_request(200);
    scale.set_value(1.0);

    let cancel_button = gtk::Button::with_label("Cancel");
    cancel_button.set_size_request(80, 30);

    let ok_button = gtk::Button::with_label("OK");
    ok_button.set_size_request(80, 30);

    {
        let column = gtk::Box::new(Orientation::Vertical, 8);
        column.set_margin(8);

        {
            let row = gtk::Box::new(Orientation::Horizontal, 4);
            row.add(&gtk::Label::new(Some("Radius")));
            row.add(&scale);
            column.add(&row);
        }
        {
            let row = gtk::Box::new(Orientation::Horizontal, 4);
            row.set_halign(gtk::Align::End);
            row.add(&cancel_button);
            row.add(&ok_button);
            column.add(&row);
        }
        dialog.add(&column);
    }

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
            if ev.button() == GDK_BUTTON_PRIMARY as u32 {
                let (x, y) = ev.position();
                tx.send(Msg::OnCanvasLeftClick(x, y)).unwrap();
            } else if ev.button() == GDK_BUTTON_SECONDARY as u32 {
                let (x, y) = ev.position();
                tx.send(Msg::OnCanvasRightClick(x, y)).unwrap();
            }
            Inhibit(false)
        }
    });

    let circles = Arc::new(Mutex::new(vec![]));
    let selected_circle = Arc::new(Mutex::new(None));

    // drawの処理はここで同期的に行う必要があり、メッセージを送るだけにはできない
    canvas.connect_draw({
        let circles = Arc::clone(&circles);

        move |canvas, ctx| {
            // マウスポインタの位置を取得する
            // (canvasの左上隅を原点する座標系で取得される)
            let mouse = (|| -> Option<(f64, f64)> {
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

            let hit = hit_test(circles, mouse).unwrap_or(usize::MAX);

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

    scale.connect_value_changed({
        let tx = tx.clone();
        move |scale| {
            let radius = scale.value();
            tx.send(Msg::OnAdjustScaleChange { radius }).unwrap();
        }
    });
    cancel_button.connect_clicked({
        let tx = tx.clone();
        move |_| tx.send(Msg::OnAdjustCancel).unwrap()
    });
    ok_button.connect_clicked({
        let tx = tx.clone();
        move |_| tx.send(Msg::OnAdjustOk).unwrap()
    });

    // ## イベントを処理する

    rx.attach(None, {
        let mut undo: Vec<Action> = vec![];
        let mut redo = vec![];

        let apply_action = {
            let circles = Arc::clone(&circles);
            move |action: &Action| match *action {
                Action::Add {
                    position: (x, y),
                    radius: r,
                } => {
                    circles.lock().unwrap().push((x, y, r));
                }
                Action::Remove { .. } => {
                    circles.lock().unwrap().pop();
                }
                Action::Resize {
                    index, new_radius, ..
                } => {
                    circles.lock().unwrap()[index].2 = new_radius;
                }
            }
        };

        move |msg| {
            eprintln!("msg {:?}", msg);

            match msg {
                Msg::OnInit => {}
                Msg::OnUndoClick => {
                    if let Some(action) = undo.pop() {
                        apply_action(&action);
                        redo.push(action.inverse());
                        canvas.queue_draw();
                    }
                }
                Msg::OnRedoClick => {
                    if let Some(action) = redo.pop() {
                        apply_action(&action);
                        undo.push(action.inverse());
                        canvas.queue_draw();
                    }
                }
                Msg::OnCanvasLeftClick(x, y) => {
                    let action = Action::Add {
                        position: (x, y),
                        radius: 20.0,
                    };
                    apply_action(&action);
                    undo.push(action.inverse());
                    redo.clear();

                    canvas.queue_draw();
                }
                Msg::OnCanvasRightClick(x, y) => {
                    let circles = circles.lock().unwrap();
                    let circles = circles.as_slice();
                    let mut selected_circle = selected_circle.lock().unwrap();

                    if let Some(hit) = hit_test(circles, (x, y)) {
                        let (_, _, r) = circles[hit];
                        *selected_circle = Some((hit, r));
                        scale.set_value(r);
                        dialog.show_all();
                        window_column.set_opacity(0.4);
                    }
                }
                Msg::OnAdjustScaleChange { radius } => {
                    let mut circles = circles.lock().unwrap();
                    let selected_circle = selected_circle.lock().unwrap();

                    let (hit, _) = selected_circle.unwrap();
                    circles[hit].2 = radius;

                    canvas.queue_draw();
                }
                Msg::OnAdjustCancel => {
                    let mut circles = circles.lock().unwrap();
                    let selected_circle = selected_circle.lock().unwrap();

                    let (hit, r) = selected_circle.unwrap();
                    let circles = circles.as_mut_slice();
                    circles[hit].2 = r;

                    canvas.queue_draw();
                    dialog.hide();
                    window_column.set_opacity(1.0);
                }
                Msg::OnAdjustOk => {
                    let action = {
                        let mut circles = circles.lock().unwrap();
                        let circles = circles.as_mut_slice();
                        let selected_circle = selected_circle.lock().unwrap();
                        let (hit, r) = selected_circle.unwrap();

                        Action::Resize {
                            index: hit,
                            old_radius: r,
                            new_radius: circles[hit].2,
                        }
                    };

                    apply_action(&action);
                    undo.push(action.inverse());
                    redo.clear();

                    dialog.hide();
                    window_column.set_opacity(1.0);
                }
            }

            undo_button.set_sensitive(!undo.is_empty());
            redo_button.set_sensitive(!redo.is_empty());
            Continue(true)
        }
    });

    // ## UIを初期化する

    tx.send(Msg::OnInit).unwrap();
    window.show_all();
}

fn hit_test(circles: &[(f64, f64, f64)], pos: (f64, f64)) -> Option<usize> {
    let (mx, my) = pos;

    circles
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
}

fn main() {
    let application = gtk::Application::new(Some("com.example.circle-drawer"), Default::default());

    application.connect_activate(|app| {
        build_ui(app);
    });

    application.run();
}
