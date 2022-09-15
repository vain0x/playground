use gtk::{prelude::*, ApplicationWindow, Orientation, WindowPosition};

fn build_ui(application: &gtk::Application) {
    let window = ApplicationWindow::new(application);

    window.set_title("Timer");
    window.set_position(WindowPosition::Center);

    let progress = gtk::ProgressBar::new();
    progress.set_valign(gtk::Align::Center);

    let progress_row = {
        let r = gtk::Box::new(Orientation::Horizontal, 8);
        r.add(&gtk::Label::new(Some("Elapsed Time:")));
        r.add(&progress);
        r
    };

    let duration_label = gtk::Label::new(Some("0.0s"));

    // let slider = gtk::Slider::new();

    let slider_row = {
        let r = gtk::Box::new(Orientation::Horizontal, 4);
        r.add(&gtk::Label::new(Some("Duration:")));
        // r.add(&slider);
        r
    };

    let reset_button = gtk::Button::with_label("Reset");

    let column = gtk::Box::new(Orientation::Vertical, 16);
    column.set_margin(24);
    column.add(&progress_row);
    column.add(&duration_label);
    column.add(&slider_row);
    column.add(&reset_button);

    window.add(&column);
    window.show_all();
}

fn main() {
    let application = gtk::Application::new(Some("com.example.timer"), Default::default());

    application.connect_activate(|app| {
        build_ui(app);
    });

    application.run();
}
