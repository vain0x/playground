//! # Cells
//!
//! 出典: [#Cells](https://eugenkiss.github.io/7guis/tasks#cells)

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

static STYLES: &[u8] = include_bytes!("styles.css");

// グリッド上のベクトル
#[derive(Debug, Clone, Copy)]
struct GridVec {
    y: u32,
    x: u32,
}

impl GridVec {
    fn new(y: u32, x: u32) -> Self {
        GridVec { y, x }
    }
}

impl std::ops::Add for GridVec {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        GridVec {
            y: self.y + rhs.y,
            x: self.x + rhs.x,
        }
    }
}

#[derive(Debug)]
enum Msg {
    OnEditBegin(GridVec),
    OnEditEnd(),
    OnEntryChanged(String),
}

#[derive(Default)]
struct State;

fn nth_alphabet(n: usize) -> char {
    (b'A' + n as u8) as char
}

fn build_ui(application: &gtk::Application) {
    // let (tx, rx) = glib::MainContext::channel(glib::PRIORITY_DEFAULT);

    // ## UIを組み立てる

    let window = ApplicationWindow::new(application);
    window.set_title("Cells");
    window.set_position(WindowPosition::Center);
    window.set_size_request(640, 480);

    let scroll = gtk::ScrolledWindow::default();
    scroll.set_widget_name("cells-scrolled-window");
    scroll.set_hexpand(true);
    scroll.set_vexpand(true);

    let grid = gtk::Grid::new();
    grid.set_widget_name("cells-grid");
    grid.set_hexpand(true);
    grid.set_vexpand(true);

    let row_count = 60;
    let column_count = 26;

    for i in 0..row_count + 1 {
        grid.insert_row(i as i32);
    }
    for i in 0..column_count + 1 {
        grid.insert_column(i as i32);
    }

    // cross header
    {
        let label = gtk::Label::new(None);
        label.set_size_request(60, 25);
        label.style_context().add_class("cross-header-cell");
        grid.attach(&label, 0, 0, 1, 1);
    }
    // row headers
    for i in 0..row_count {
        let label = gtk::Label::new(Some(&format!("{}", i + 1)));
        label.set_size_request(60, 25);
        label.set_xalign(0.75);
        label.style_context().add_class("row-header-cell");
        grid.attach(&label, 0, (1 + i) as i32, 1, 1);
    }
    // column headers
    for i in 0..column_count {
        let label = gtk::Label::new(Some(&format!("{}", nth_alphabet(i))));
        label.set_size_request(60, 25);
        label.style_context().add_class("column-header-cell");
        grid.attach(&label, (1 + i) as i32, 0, 1, 1);
    }
    // data cells
    for y in 0..row_count {
        for x in 0..column_count {
            let label = gtk::Label::new(Some(&format!("{},{}", nth_alphabet(x), y + 1)));
            label.set_size_request(60, 25);
            label.style_context().add_class("cell-label");

            if y == 0 {
                label.style_context().add_class("first-row-cell");
            }
            if x == 0 {
                label.style_context().add_class("first-column-cell");
            }

            grid.attach(&label, (1 + x) as i32, (1 + y) as i32, 1, 1);
        }
    }

    scroll.add(&grid);
    window.add(&scroll);

    // ## UIのイベントにコールバックを接続する

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
