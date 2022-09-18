//! # CRUD

// 参考:
//
// - リストボックス
//      https://docs.gtk.org/gtk3/class.ListBox.html
//      - 項目として文字列を挿入するには、ラベルに包んでから挿入する
//      - 行を挿入した後、挿入された行を `show_all` によって表示する (そうでないと中身が空欄になる)

use gtk::{glib, prelude::*, ApplicationWindow, Orientation, WindowPosition};

#[derive(Debug)]
enum Msg {
    OnInit,
    OnFilterChanged(String),
    OnListRowSelected(i32),
    OnCreateClick,
    OnUpdateClick,
    OnDeleteClick,
}

fn build_ui(application: &gtk::Application) {
    let (tx, rx) = glib::MainContext::channel(glib::PRIORITY_DEFAULT);

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

    // TODO: リストボックスの高さの最小値を決める、要素数が多いときはスクロールする
    let listbox = gtk::ListBox::new();
    {
        listbox.set_widget_name("persons-listbox");
        listbox.set_vexpand(true);
        listbox.set_can_focus(false);
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

    {
        let row = gtk::Box::new(Orientation::Horizontal, 16);
        row.add(&create_button);
        row.add(&update_button);
        row.add(&delete_button);
        grid.attach(&row, 0, y, W, 1);
    }

    window.add(&grid);

    // ## UIのイベントにコールバックを接続する

    filter_entry.connect_changed({
        let tx = tx.clone();
        move |entry| {
            let value = entry.buffer().text();
            tx.send(Msg::OnFilterChanged(value)).unwrap();
        }
    });
    listbox.connect_row_selected({
        let tx = tx.clone();
        move |_, row_opt| {
            let value = match row_opt {
                Some(row) => row.index(),
                None => -1,
            };
            tx.send(Msg::OnListRowSelected(value)).unwrap();
        }
    });
    // name_entry.connect_changed({
    //     let tx = tx.clone();
    //     move |entry| {
    //         let value = entry.buffer().text();
    //         tx.send(Msg::OnNameChanged(value)).unwrap();
    //     }
    // });
    // surname_entry.connect_changed({
    //     let tx = tx.clone();
    //     move |entry| {
    //         let value = entry.buffer().text();
    //         tx.send(Msg::OnSurnameChanged(value)).unwrap();
    //     }
    // });
    create_button.connect_clicked({
        let tx = tx.clone();
        move |_| tx.send(Msg::OnCreateClick).unwrap()
    });
    update_button.connect_clicked({
        let tx = tx.clone();
        move |_| tx.send(Msg::OnUpdateClick).unwrap()
    });
    delete_button.connect_clicked({
        let tx = tx.clone();
        move |_| tx.send(Msg::OnDeleteClick).unwrap()
    });

    // ## イベントを処理する

    rx.attach(None, {
        let _tx = tx.clone();

        // リストボックスの行コンテナからテキストを取り出す
        fn row_to_string(row: &gtk::ListBoxRow) -> glib::GString {
            row.child()
                .unwrap()
                .downcast::<gtk::Label>()
                .unwrap()
                .text()
        }

        move |msg| {
            eprintln!("msg {:?}", msg);

            match msg {
                Msg::OnInit => {
                    let data = ["Emil, Hans", "Mustermann, Max", "Tisch, Roman"];
                    for (i, name) in data.iter().enumerate() {
                        listbox.insert(&gtk::Label::new(Some(&name)), i as i32);
                    }
                    listbox.select_row(listbox.row_at_index(0).as_ref());
                    listbox.show_all();
                }
                Msg::OnFilterChanged(filter) => {
                    if !filter.is_empty() {
                        let filter_fn =
                            move |row: &gtk::ListBoxRow| row_to_string(row).contains(&filter);
                        listbox.set_filter_func(Some(Box::new(filter_fn)));
                    } else {
                        listbox.set_filter_func(None);
                    }
                }
                Msg::OnListRowSelected(index) => {
                    let item = listbox.row_at_index(index).map(|row| row_to_string(&row));
                    let (name, surname) = match &item {
                        Some(item) => item.split_once(", ").unwrap(),
                        None => ("", ""),
                    };

                    name_entry.buffer().set_text(name);
                    surname_entry.buffer().set_text(surname);
                }
                Msg::OnCreateClick => {
                    let item = {
                        let name = name_entry.buffer().text();
                        let surname = surname_entry.buffer().text();
                        format!("{}, {}", name, surname)
                    };

                    let position = listbox.children().len() as i32;
                    listbox.insert(&gtk::Label::new(Some(&item)), position);

                    // 新しい行を選択する
                    if let Some(row) = listbox.row_at_index(position) {
                        listbox.select_row(Some(&row));
                        row.show_all();
                    }
                }
                Msg::OnUpdateClick => {
                    let item = {
                        let name = name_entry.buffer().text();
                        let surname = surname_entry.buffer().text();
                        format!("{}, {}", name, surname)
                    };

                    if let Some(row) = listbox.selected_row() {
                        // 行コンテナの要素であるラベルを置き換える (もっといい方法がありそう)
                        row.remove(&row.child().unwrap());
                        row.add(&gtk::Label::new(Some(&item)));
                        row.show_all();
                    }

                    listbox.invalidate_filter();
                }
                Msg::OnDeleteClick => {
                    if let Some(row) = listbox.selected_row() {
                        let position = row.index();
                        listbox.remove(&row);

                        // 次の行か末尾の行を改めて選択する (これがないと選択された行がない状態になる)
                        if let Some(row) = listbox
                            .row_at_index(position)
                            .or_else(|| listbox.row_at_index(position - 1))
                        {
                            listbox.select_row(Some(&row));
                        }
                    }
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
    let application = gtk::Application::new(Some("com.example.crud"), Default::default());

    application.connect_activate(|app| {
        build_ui(app);
    });

    application.run();
}
