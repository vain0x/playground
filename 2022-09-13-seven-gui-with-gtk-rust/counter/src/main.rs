use gtk::{prelude::*, ApplicationWindow, Orientation, WindowPosition};

fn build_ui(application: &gtk::Application) {
    let window = ApplicationWindow::new(application);

    window.set_title("Counter");
    window.set_position(WindowPosition::Center);

    let entry = gtk::Entry::new();
    entry.set_width_request(120);
    // entry.set_justification(gtk::Justification::Right);
    let buffer = entry.buffer();
    buffer.set_text("0");

    let button = gtk::Button::new();
    button.set_label("Count");
    button.connect_clicked({
        let buffer = buffer.clone();
        move |_| {
            let text = buffer.text();
            if let Ok(value) = text.parse::<i32>() {
                buffer.set_text(&(value + 1).to_string());
            }
        }
    });

    let row = gtk::Box::new(Orientation::Horizontal, 8);
    row.set_margin(16);
    row.add(&entry);
    row.add(&button);

    window.add(&row);
    window.show_all();
}

fn main() {
    let application = gtk::Application::new(Some("com.example.counter"), Default::default());

    application.connect_startup(|app| {
        build_ui(app);
    });

    application.run();
}
