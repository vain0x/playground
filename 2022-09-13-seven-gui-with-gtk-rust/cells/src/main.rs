//! # Cells
//!
//! 出典: [#Cells](https://eugenkiss.github.io/7guis/tasks#cells)

mod coord;
mod formula;
mod model;

use coord::*;
use gtk::{gdk, prelude::*, ApplicationWindow, WindowPosition};
use std::{cell::RefCell, rc::Rc};

static STYLES: &[u8] = include_bytes!("styles.css");

#[allow(unused)]
#[derive(Debug)]
enum Msg {
    OnEditBegin(GridVec),
    OnEditEnd,
    OnEntryChanged(String),
}

#[derive(Default)]
struct State;

struct Sheet {
    #[allow(unused)]
    size: GridVec,
    inputs: Vec<Vec<String>>,

    editor_rect: Option<gtk::Rectangle>,
    edit_state: Option<EditState>,
}

impl Sheet {
    fn new(size: GridVec) -> Self {
        let (h, w) = size.pair();

        Self {
            size,
            inputs: vec![vec!["".into(); h]; w],
            editor_rect: None,
            edit_state: None,
        }
    }
}

struct EditState {
    pos: GridVec,
    on_changed: Box<dyn Fn(&mut Sheet, &EditState, String) + 'static>,
}

fn nth_alphabet(n: usize) -> char {
    (b'A' + n as u8) as char
}

fn build_ui(application: &gtk::Application) {
    let row_count = 100;
    let column_count = 26;
    let size = GridVec::from((column_count, row_count));

    let sheet_ref: Rc<RefCell<Sheet>> = Rc::new(RefCell::new(Sheet::new(size)));

    {
        let mut sheet = sheet_ref.borrow_mut();
        for y in 0..row_count {
            for x in 0..column_count {
                sheet.inputs[y][x] = format!("{},{}", nth_alphabet(x), y + 1);
            }
        }
    }

    // let (tx, rx) = glib::MainContext::channel(glib::PRIORITY_DEFAULT);

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
        let sheet = sheet_ref.borrow();
        for y in 0..row_count {
            for x in 0..column_count {
                let label = gtk::Label::new(Some(&sheet.inputs[y][x]));
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
                    let editor = editor.clone();
                    let label = label.clone();
                    let overlay = overlay.clone();
                    let scroll = scroll.clone();
                    let sheet_ref = Rc::clone(&sheet_ref);

                    move |eb, ev| {
                        if ev.button() != gdk::BUTTON_PRIMARY {
                            return Inhibit(false);
                        }

                        let label = label.clone();
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
                            let v = GridVec::from((y, x));
                            let mut sheet = sheet_ref.borrow_mut();
                            input = sheet.inputs[y][x].clone();
                            sheet.editor_rect = Some(size);
                            sheet.edit_state = Some(EditState {
                                pos: v,
                                on_changed: Box::new(move |sheet, state, text| {
                                    let (y, x) = state.pos.pair();
                                    label.set_text(&text);
                                    sheet.inputs[y][x] = text;
                                }),
                            });
                        };

                        // Update view:

                        editor.buffer().set_text(&input);
                        editor.set_visible(true);
                        editor.set_allocation(&size);
                        editor.set_has_focus(true);
                        overlay.queue_resize();
                        Inhibit(true)
                    }
                });

                grid.attach(&eb, (1 + x) as i32, (1 + y) as i32, 1, 1);
                data_cells[y].push(label);
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
        let sheet_ref = Rc::clone(&sheet_ref);

        move |_, _| {
            sheet_ref.borrow_mut().editor_rect = None;
            editor.set_visible(false);
            Inhibit(false)
        }
    });

    // オーバーレイ上のマウス操作が下にあるグリッドに貫通 (pass-through) するように設定する
    overlay.connect_realize(|p| {
        p.window().unwrap().set_pass_through(true);
    });

    // オーバーレイ上のエディタの表示位置を絶対座標で決める
    overlay.connect_get_child_position({
        let sheet_ref = Rc::clone(&sheet_ref);
        move |_, _| sheet_ref.borrow().editor_rect
    });

    // editorのEscapeキーでエディタを非表示にする
    editor.connect_key_press_event({
        let sheet_ref = Rc::clone(&sheet_ref);

        move |editor, ev| {
            let mut close = false;

            if ev.keyval() == gdk::keys::constants::Return {
                close = true;

                let input = editor.buffer().text();
                let mut sheet = sheet_ref.borrow_mut();
                if let Some(edit) = sheet.edit_state.take() {
                    (edit.on_changed)(&mut *sheet, &edit, input);
                }
            } else if ev.keyval() == gdk::keys::constants::Escape {
                close = true;
            }

            if close {
                sheet_ref.borrow_mut().editor_rect = None;
                editor.set_visible(false);
            }

            Inhibit(false)
        }
    });

    // ## イベントを処理する

    // rx.attach(None, {
    //     let mut state = State::default();

    //     move |msg| {
    //         eprintln!("msg {:?}", msg);

    //         match msg {
    //             Msg::OnInit => {}
    //         }
    //         Continue(true)
    //     }
    // });

    // ## UIを初期化する

    // tx.send(Msg::OnInit).unwrap();
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
