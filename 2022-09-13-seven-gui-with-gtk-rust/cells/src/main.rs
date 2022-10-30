//! # Cells
//!
//! 出典: [#Cells](https://eugenkiss.github.io/7guis/tasks#cells)

mod coord;
mod formula;
mod model;

use coord::*;
use gtk::{gdk, glib, prelude::*, ApplicationWindow, WindowPosition};
use model::TableData;
use std::{cell::RefCell, rc::Rc};

static STYLES: &[u8] = include_bytes!("styles.css");

struct Sheet {
    inputs: GridArray<String>,
    table: TableData,
    editing_cell_opt: Option<Coord>,
}

impl Sheet {
    fn new(size: Coord) -> Self {
        Self {
            inputs: GridArray::new(size),
            table: TableData::new(size),
            editing_cell_opt: None,
        }
    }
}

#[derive(Default)]
struct ViewState {
    editor_rect_opt: Option<gtk::Rectangle>,
}

#[derive(Debug)]
enum Msg {
    OnCellClick(Coord),
    OnEditEnd { input: String },
}

fn nth_alphabet(n: usize) -> char {
    (b'A' + n as u8) as char
}

fn build_ui(application: &gtk::Application) {
    let row_count = 100;
    let column_count = 26;
    let size = Coord::from((row_count, column_count));

    let view_state_ref: Rc<RefCell<ViewState>> = Rc::default();
    let mut sheet = Sheet::new(size);

    {
        for y in 0..row_count {
            for x in 0..column_count {
                let text = format!("{},{}", nth_alphabet(x), y + 1);
                let pos = Coord::from((y, x));
                sheet.table.set(pos, &text);
                sheet.inputs[pos] = text;
            }
        }

        // 数式の記述例を初期状態に書き込んでおく:
        for &(y, input) in &[
            (1, "Examples:"),
            (2, ";(add);20;+;22;->;=add(C2,E2)"),
            (3, ";(sum);1;2;3;->;=sum(C3:E3)"),
        ] {
            for (x, input) in input.split(';').enumerate() {
                if input.is_empty() {
                    continue;
                }
                let p = Coord::new(y as u32, x as u32);
                sheet.table.set(p, input);
                sheet.inputs[p] = input.into();
            }
        }

        sheet.table.update();

        for p in Vec::from_iter(sheet.table.drain_changes()) {
            let value = sheet.table.value_at(p);
            sheet.inputs[p] = value.to_string();
        }
    }

    let (tx, rx) = glib::MainContext::channel(glib::PRIORITY_DEFAULT);

    // ## UIを組み立てる

    //  window {
    //      overlay {
    //          editor (overlay)
    //          scroll {
    //              grid
    //          }
    //      }
    //  }

    let window = ApplicationWindow::new(application);
    window.set_title("Cells");
    window.set_default_size(640, 480);
    window.set_position(WindowPosition::Center);

    // UI要素を重ね合わせて表示するためのコンポーネント
    let overlay = gtk::Overlay::default();
    overlay.set_vexpand(true);
    overlay.set_hexpand(true);

    // セルの編集時に表示される入力欄
    let editor = gtk::Entry::new();
    editor.set_widget_name("cells-editor");
    editor.set_visible(false);
    editor.set_no_show_all(true);

    let scroll = gtk::ScrolledWindow::default();
    scroll.set_widget_name("cells-scrolled-window");
    scroll.set_hexpand(true);
    scroll.set_vexpand(true);

    let grid = gtk::Grid::new();
    grid.set_widget_name("cells-grid");
    grid.set_hexpand(true);
    grid.set_vexpand(true);

    for i in 0..row_count + 1 {
        grid.insert_row(i as i32);
    }
    for i in 0..column_count + 1 {
        grid.insert_column(i as i32);
    }

    let _cross_header = {
        let label = gtk::Label::new(None);
        label.set_size_request(60, 25);
        label.style_context().add_class("cross-header-cell");

        grid.attach(&label, 0, 0, 1, 1);
        label
    };

    let _row_headers = {
        let mut labels = vec![];
        for y in 0..row_count {
            let label = gtk::Label::new(Some(&format!("{}", y)));
            label.set_size_request(60, 25);
            label.set_xalign(0.75);
            label.style_context().add_class("row-header-cell");

            grid.attach(&label, 0, (1 + y) as i32, 1, 1);
            labels.push(label);
        }
        labels
    };

    let _column_headers = {
        let mut labels = vec![];
        for x in 0..column_count {
            let label = gtk::Label::new(Some(&format!("{}", nth_alphabet(x))));
            label.set_size_request(60, 25);
            label.style_context().add_class("column-header-cell");

            grid.attach(&label, (1 + x) as i32, 0, 1, 1);
            labels.push(label);
        }
        labels
    };

    let mut data_cells = vec![vec![]; row_count];

    {
        for y in 0..row_count {
            for x in 0..column_count {
                let pos = Coord::from((y, x));
                let label = gtk::Label::new(Some(&sheet.inputs[pos]));
                label.set_size_request(60, 25);
                label.style_context().add_class("cell-label");

                if y == 0 {
                    label.style_context().add_class("first-row-cell");
                }
                if x == 0 {
                    label.style_context().add_class("first-column-cell");
                }

                let eb = gtk::EventBox::new();
                eb.add(&label);

                eb.connect_button_press_event({
                    let tx = tx.clone();

                    move |_, ev| {
                        if ev.button() == gdk::BUTTON_PRIMARY {
                            tx.send(Msg::OnCellClick(Coord::from((y, x)))).unwrap();
                            Inhibit(true)
                        } else {
                            Inhibit(false)
                        }
                    }
                });

                grid.attach(&eb, (1 + x) as i32, (1 + y) as i32, 1, 1);
                data_cells[y].push((eb, label));
            }
        }
    }

    scroll.add(&grid);
    overlay.add_overlay(&editor);
    overlay.add(&scroll);
    window.add(&overlay);

    // ## UIのイベントにコールバックを接続する

    // オーバーレイの外側のクリックでエディタを非表示にする
    window.connect_button_press_event({
        let editor = editor.clone();
        let tx = tx.clone();
        let view_state_ref = Rc::clone(&view_state_ref);

        move |_, _| {
            if gtk::prelude::EntryExt::is_visible(&editor) {
                let input = editor.buffer().text();
                view_state_ref.borrow_mut().editor_rect_opt = None;
                editor.set_visible(false);
                tx.send(Msg::OnEditEnd { input }).unwrap();
            }

            Inhibit(false)
        }
    });

    // オーバーレイ上のマウス操作が下にあるグリッドに貫通 (pass-through) するように設定する
    overlay.connect_realize(|overlay| {
        overlay.window().unwrap().set_pass_through(true);
    });

    // オーバーレイ上のエディタの表示位置を絶対座標で決める
    overlay.connect_get_child_position({
        let view_state_ref = Rc::clone(&view_state_ref);
        move |_, _| view_state_ref.borrow().editor_rect_opt
    });

    // editorのEscapeキーでエディタを非表示にする
    editor.connect_key_press_event({
        let tx = tx.clone();
        let view_state_ref = Rc::clone(&view_state_ref);

        move |editor, ev| {
            let mut close = false;

            if ev.keyval() == gdk::keys::constants::Return {
                close = true;
                let input = editor.buffer().text();
                tx.send(Msg::OnEditEnd { input }).unwrap();
            } else if ev.keyval() == gdk::keys::constants::Escape {
                close = true;
                let input = editor.buffer().text();
                tx.send(Msg::OnEditEnd { input }).unwrap();
            }

            if close {
                view_state_ref.borrow_mut().editor_rect_opt = None;
                editor.set_visible(false);
            }

            Inhibit(false)
        }
    });

    // ## イベントを処理する

    rx.attach(None, {
        // moves sheet
        move |msg| {
            eprintln!("msg {:?}", msg);

            match msg {
                Msg::OnCellClick(p) => {
                    let (y, x) = p.pair();
                    let (eb, _) = &data_cells[y][x];
                    let rect = eb.allocation();
                    eprintln!("click ({y}, {x}) ({}x{})", rect.width(), rect.height());

                    let p = 4; // margin, padding of grid
                    let offset_y = scroll.vadjustment().value() as i32;
                    let offset_x = scroll.hadjustment().value() as i32;
                    let size = gtk::Rectangle::new(
                        rect.x() - offset_x + p,
                        rect.y() - offset_y + p,
                        rect.width().max(100),
                        rect.height(),
                    );

                    // Update model:

                    let input;
                    {
                        let pos = Coord::from((y, x));
                        input = sheet.inputs[y][x].clone();
                        sheet.editing_cell_opt = Some(pos);
                    };

                    // Update view:

                    view_state_ref.borrow_mut().editor_rect_opt = Some(size);

                    editor.buffer().set_text(&input);
                    editor.set_visible(true);
                    editor.set_allocation(&size);
                    editor.set_has_focus(true);
                    overlay.queue_resize();
                }
                Msg::OnEditEnd { input } => {
                    if let Some(pos) = sheet.editing_cell_opt.take() {
                        // Update cell:
                        {
                            let (y, x) = pos.pair();
                            sheet.table.set(pos, &input);
                            sheet.inputs[y][x] = input;
                        }

                        // Propagate changes:
                        sheet.table.update();

                        for pos in Vec::from_iter(sheet.table.drain_changes()) {
                            let (y, x) = pos.pair();
                            let (_, label) = &data_cells[y][x];
                            label.set_text(&sheet.table.value_at(pos).to_string());
                        }
                    }
                }
            }
            Continue(true)
        }
    });

    // ## UIを初期化する

    window.show_all();
}

fn main() {
    let application = gtk::Application::new(Some("com.example.cells"), Default::default());

    application.connect_activate(|app| {
        let provider = gtk::CssProvider::new();
        provider
            .load_from_data(STYLES)
            .expect("CssProvider::load_from_data");

        gtk::StyleContext::add_provider_for_screen(
            &gdk::Screen::default().expect("Screen::default"),
            &provider,
            gtk::STYLE_PROVIDER_PRIORITY_APPLICATION,
        );

        build_ui(app);
    });

    application.run();
}
