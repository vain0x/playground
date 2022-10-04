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
    OnAdjustOk,
}

#[derive(Clone, Copy)]
struct Circle {
    x: f64,
    y: f64,
    r: f64,
}

#[derive(Default)]
struct State {
    circles: Vec<Circle>,
    // (index, old_radius)
    selected_circle_opt: Option<(usize, f64)>,
}

enum Action {
    Add(Circle),
    Remove(Circle),
    Resize {
        index: usize,
        old_radius: f64,
        new_radius: f64,
    },
}

impl Action {
    fn inverse(self) -> Action {
        match self {
            Action::Add(c) => Action::Remove(c),
            Action::Remove(c) => Action::Add(c),
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

fn apply_action(state: &mut State, action: &Action) {
    match *action {
        Action::Add(c) => {
            state.circles.push(c);
        }
        Action::Remove(_) => {
            state.circles.pop();
        }
        Action::Resize {
            index, new_radius, ..
        } => {
            state.circles[index].r = new_radius;
        }
    }
}

fn hit_test(circles: &[Circle], pos: (f64, f64)) -> Option<usize> {
    let (mx, my) = pos;

    circles.iter().enumerate().rev().find_map(|(i, c)| {
        if f64::hypot(mx - c.x, my - c.y) < c.r + 1e-6 {
            Some(i)
        } else {
            None
        }
    })
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
    dialog.set_title("Resizing Circle");
    dialog.set_transient_for(Some(&window));
    dialog.set_position(WindowPosition::CenterOnParent);
    dialog.set_modal(true);

    let dialog_label = gtk::Label::new(None);

    let scale = gtk::Scale::with_range(Orientation::Horizontal, 1.0, 30.0, 0.5);
    scale.set_width_request(200);
    scale.set_value(1.0);

    {
        let column = gtk::Box::new(Orientation::Vertical, 8);
        column.set_margin_start(32);
        column.set_margin_end(32);
        column.set_margin_top(16);
        column.set_margin_bottom(16);

        dialog_label.set_halign(gtk::Align::Start);
        column.add(&dialog_label);

        scale.set_halign(gtk::Align::Start);
        column.add(&scale);

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

    let store = Arc::new(Mutex::new(State::default()));

    // drawの処理はここで同期的に行う必要があり、メッセージを送るだけにはできない
    canvas.connect_draw({
        let store = Arc::clone(&store);

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

            let state = store.lock().unwrap();

            let hit = hit_test(&state.circles, mouse).unwrap_or(usize::MAX);

            ctx.set_line_width(1.0);

            for (i, &c) in state.circles.iter().enumerate() {
                let Circle { x: cx, y: cy, r } = c;

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

    // 閉じるボタンが押されたときに呼ばれる
    // (Inhibit(true)を返すことで既定の挙動をキャンセルする。
    //  そうしないとダイアログが破棄されてしまい、再度開くことができない)
    dialog.connect_delete_event({
        let tx = tx.clone();
        move |_, _| {
            tx.send(Msg::OnAdjustOk).unwrap();
            Inhibit(true)
        }
    });

    // ## イベントを処理する

    rx.attach(None, {
        let mut undo: Vec<Action> = vec![];
        let mut redo: Vec<Action> = vec![];

        move |msg| {
            eprintln!("msg {:?}", msg);

            match msg {
                Msg::OnInit => {}
                Msg::OnUndoClick => {
                    let mut state = store.lock().unwrap();

                    if let Some(action) = undo.pop() {
                        apply_action(&mut state, &action);
                        redo.push(action.inverse());
                        canvas.queue_draw();
                    }
                }
                Msg::OnRedoClick => {
                    let mut state = store.lock().unwrap();

                    if let Some(action) = redo.pop() {
                        apply_action(&mut state, &action);
                        undo.push(action.inverse());
                        canvas.queue_draw();
                    }
                }
                Msg::OnCanvasLeftClick(x, y) => {
                    let mut state = store.lock().unwrap();
                    let action = Action::Add(Circle { x, y, r: 20.0 });
                    apply_action(&mut state, &action);
                    undo.push(action.inverse());
                    redo.clear();

                    canvas.queue_draw();
                }
                Msg::OnCanvasRightClick(x, y) => {
                    let mut state = store.lock().unwrap();

                    if let Some(hit) = hit_test(&state.circles, (x, y)) {
                        let Circle { x, y, r } = state.circles[hit];
                        state.selected_circle_opt = Some((hit, r));
                        dialog_label
                            .set_text(&format!("Adjust radius of circle at ({x:.0}, {y:.0})"));
                        scale.set_value(r);
                        dialog.show_all();
                        window_column.set_opacity(0.4);
                    }
                }
                Msg::OnAdjustScaleChange { radius } => {
                    let mut state = store.lock().unwrap();

                    let (hit, _) = state.selected_circle_opt.unwrap();
                    state.circles[hit].r = radius;

                    canvas.queue_draw();
                }
                Msg::OnAdjustOk => {
                    let action = {
                        let state = store.lock().unwrap();
                        let (hit, r) = state.selected_circle_opt.unwrap();

                        Action::Resize {
                            index: hit,
                            old_radius: r,
                            new_radius: state.circles[hit].r,
                        }
                    };

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

fn main() {
    let application = gtk::Application::new(Some("com.example.circle-drawer"), Default::default());

    application.connect_activate(|app| {
        build_ui(app);
    });

    application.run();
}
