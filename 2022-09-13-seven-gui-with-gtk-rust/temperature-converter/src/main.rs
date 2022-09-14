use gtk::{prelude::*, ApplicationWindow, Orientation, WindowPosition};
use std::{cell::RefCell, rc::Rc};

fn build_ui(application: &gtk::Application) {
    let window = ApplicationWindow::new(application);

    window.set_title("Temperature Converter");
    window.set_position(WindowPosition::Center);

    // celsius
    let c_entry = gtk::Entry::new();
    c_entry.set_width_request(60);
    c_entry.buffer().set_text("0");

    let label1 = gtk::Label::new(Some("Celsius ="));
    label1.set_margin_end(8);

    // fahrenheit
    let f_entry = gtk::Entry::new();
    f_entry.set_width_request(60);
    f_entry.buffer().set_text("0");

    let label2 = gtk::Label::new(Some("Fahrenheit"));

    let row = gtk::Box::new(Orientation::Horizontal, 4);
    row.set_margin(16);
    row.add(&c_entry);
    row.add(&label1);
    row.add(&f_entry);
    row.add(&label2);

    // 一方が変更中かどうか
    //
    // 再帰的な変更を防ぐため。これがないと一方の値を変更したときに他方の値を更新し、それによって入力側が上書きされてしまう
    let changing = Rc::new(RefCell::new(false));

    c_entry.connect_changed({
        let f_entry = f_entry.clone();
        let changing = changing.clone();

        move |c_entry| {
            if std::mem::replace(&mut *changing.borrow_mut(), true) {
                return;
            }

            eprintln!("begin C change");
            if let Ok(c) = c_entry.buffer().text().parse::<f64>() {
                let f = c * 9.0 / 5.0 + 32.0;
                f_entry.set_text(&format!("{:.2}", f));
            } else if c_entry.buffer().text().is_empty() {
                f_entry.set_text("0");
            }

            *changing.borrow_mut() = false;
            eprintln!("end C change");
        }
    });

    // Self-adjust on focus lost.
    c_entry.connect_focus_out_event(|c_entry, _| {
        eprintln!("C focus lost");
        let value = match c_entry.buffer().text().parse::<f64>() {
            Ok(value) => value,
            Err(_) => 0.0,
        };
        c_entry.set_text(&format!("{:.2}", value));
        Inhibit(false)
    });

    f_entry.connect_changed({
        let c_entry = c_entry.clone();
        let changing = Rc::clone(&changing);

        move |f_entry| {
            if std::mem::replace(&mut *changing.borrow_mut(), true) {
                return;
            }

            eprintln!("begin F change");
            if let Ok(f) = f_entry.buffer().text().parse::<f64>() {
                let c = (f - 32.0) * 5.0 / 9.0;
                c_entry.set_text(&format!("{:.2}", c));
            }

            *changing.borrow_mut() = false;
            eprintln!("end F change");
        }
    });

    f_entry.connect_focus_out_event(|f_entry, _| {
        eprintln!("C focus lost");
        let value = match f_entry.buffer().text().parse::<f64>() {
            Ok(value) => value,
            Err(_) => 0.0,
        };
        f_entry.set_text(&format!("{:.2}", value));
        Inhibit(false)
    });

    window.add(&row);
    window.show_all();
}

fn main() {
    let application = gtk::Application::new(
        Some("com.example.temperature-converter"),
        Default::default(),
    );

    application.connect_startup(|app| {
        build_ui(app);
    });

    application.run();
}
