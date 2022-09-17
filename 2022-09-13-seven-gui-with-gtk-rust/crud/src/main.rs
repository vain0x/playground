//! # CRUD

// 参考:
//
// - https://docs.gtk.org/gtk3/class.ListBox.html

use gtk::{prelude::*, ApplicationWindow, Orientation, WindowPosition};

fn build_ui(application: &gtk::Application) {
    // ## UIを組み立てる

    let window = ApplicationWindow::new(application);
    window.set_title("CRUD");
    window.set_position(WindowPosition::Center);

    let grid = gtk::Grid::new();
    grid.set_margin(24);

    // 行・列の間隔
    grid.set_row_spacing(8);
    grid.set_column_spacing(8);

    // 親要素に合わせて拡大する
    grid.set_hexpand(true);
    grid.set_vexpand(true);

    // 列数
    const W: i32 = 4;

    let mut y = 0;

    let filter_entry = {
        let e = gtk::Entry::new();
        grid.attach(&gtk::Label::new(Some("Filter prefix:")), 0, y, 1, 1);
        grid.attach(&e, 1, 0, 1, 1);
        e
    };
    y += 1;

    let listbox = gtk::ListBox::new();
    {
        for (i, &item) in ["Emil, Hans", "Mustermann, Max", "Tisch, Roman"]
            .iter()
            .enumerate()
        {
            listbox.insert(&gtk::Label::new(Some(item)), i as i32);
        }

        listbox.set_widget_name("persons-listbox");
        listbox.set_vexpand(true);
        grid.attach(&listbox, 0, y, 2, 3);
    }

    let name_entry = {
        let l = gtk::Label::new(Some("Name:"));
        let e = gtk::Entry::new();
        grid.attach(&l, 2, y, 1, 1);
        grid.attach(&e, 3, y, 1, 1);
        e
    };
    y += 1;

    let surname_entry = {
        let l = gtk::Label::new(Some("Surname:"));
        let e = gtk::Entry::new();
        grid.attach(&l, 2, y, 1, 1);
        grid.attach(&e, 3, y, 1, 1);
        e
    };
    y += 1;

    // ウィンドウが拡大しても名前の入力欄が拡大しないように、ダミーの拡大可能な行を配置する
    let dummy_cell = gtk::Box::new(Orientation::Horizontal, 0);
    dummy_cell.set_vexpand(true);
    grid.attach(&dummy_cell, 0, y, W, 1);
    y += 1;

    let create_button = gtk::Button::with_label("Create");
    let update_button = gtk::Button::with_label("Update");
    let delete_button = gtk::Button::with_label("Delete");

    let buttons_row = {
        let r = gtk::Box::new(Orientation::Horizontal, 16);
        r.add(&create_button);
        r.add(&update_button);
        r.add(&delete_button);
        grid.attach(&r, 0, 4, W, 1);
        r
    };

    window.add(&grid);

    // ## UIのイベントにコールバックを接続する

    // TODO

    // ## UIを初期化する

    window.show_all();
}

fn main() {
    let application = gtk::Application::new(Some("com.example.crud"), Default::default());

    application.connect_activate(|app| {
        build_ui(app);
    });

    application.run();
}
