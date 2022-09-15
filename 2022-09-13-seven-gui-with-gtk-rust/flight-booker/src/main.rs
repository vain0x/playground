use gtk::{gdk, prelude::*, ApplicationWindow, Orientation, WindowPosition};
use std::cmp::Ordering;

static STYLES: &[u8] = include_bytes!("styles.css");

fn build_ui(application: &gtk::Application) {
    let window = ApplicationWindow::new(application);

    window.set_title("Flight Booker");
    window.set_default_width(400);
    window.set_position(WindowPosition::Center);

    let mode_combobox = gtk::ComboBoxText::new();
    mode_combobox.append_text("one-way flight");
    mode_combobox.append_text("return flight");
    mode_combobox.set_active(Some(0));

    let start_date = gtk::Entry::new();
    start_date.buffer().set_text("2001-01-01");
    let end_date = gtk::Entry::new();
    end_date.buffer().set_text("2001-01-01");
    end_date.set_margin_bottom(8);

    let book_button = gtk::Button::with_label("Book");

    let refresh_end_date_sensitive = {
        let mode_combobox = mode_combobox.clone();
        let end_date = end_date.clone();

        move || {
            end_date.set_sensitive(mode_combobox.active().map_or(false, |index| index == 1));
        }
    };

    let refresh_button_sensitive = {
        let mode_combobox = mode_combobox.clone();
        let start_date = start_date.clone();
        let end_date = end_date.clone();
        let book_button = book_button.clone();

        move || {
            let mode_selected = mode_combobox.active().is_some();
            let is_return_flight = mode_combobox.active().map_or(false, |index| index == 1);
            let s = parse_date(&start_date.buffer().text());
            let t = parse_date(&end_date.buffer().text());
            let valid_interval = {
                match (s, t) {
                    (Some(s), Some(t)) => s.cmp(&t).is_le(),
                    _ => false,
                }
            };
            // eprintln!(
            //     "button sensitive? mode={:?} s={} t={} {} {}",
            //     mode_combobox.active(),
            //     start_date.buffer().text(),
            //     end_date.buffer().text(),
            //     s.map_or("None".into(), |x| x.to_string()),
            //     t.map_or("None".into(), |x| x.to_string())
            // );
            let ok = mode_selected && s.is_some() && (!is_return_flight || valid_interval);
            book_button.set_sensitive(ok);
        }
    };

    mode_combobox.connect_changed({
        let refresh_button_sensitive = refresh_button_sensitive.clone();
        let refresh_end_date_sensitive = refresh_end_date_sensitive.clone();

        move |_| {
            refresh_button_sensitive();
            refresh_end_date_sensitive();
        }
    });

    start_date.connect_changed({
        let refresh_button_sensitive = refresh_button_sensitive.clone();

        move |start_date| {
            refresh_button_sensitive();

            // 検証エラーが発生しているときだけクラスをつける。CSSにおいて `.invalid` にマッチするようになる
            let ok = parse_date(&start_date.buffer().text()).is_some();
            if ok {
                start_date.style_context().remove_class("invalid");
            } else {
                start_date.style_context().add_class("invalid");
            }
        }
    });

    end_date.connect_changed({
        let refresh_button_sensitive = refresh_button_sensitive.clone();

        move |end_date| {
            refresh_button_sensitive();

            let ok = parse_date(&end_date.buffer().text()).is_some();
            if ok {
                end_date.style_context().remove_class("invalid");
            } else {
                end_date.style_context().add_class("invalid");
            }
        }
    });

    book_button.connect_clicked({
        let mode_combobox = mode_combobox.clone();
        let start_date = start_date.clone();
        let end_date = end_date.clone();

        move |_| {
            let s = start_date.buffer().text();
            let t = end_date.buffer().text();
            let message = match mode_combobox.active() {
                Some(0) => format!("one-way flight on {s}"),
                Some(1) => format!("return flight from {s} to {t}"),
                _ => return,
            };
            println!("You have booked {message}.");
        }
    });

    let column = gtk::Box::new(Orientation::Vertical, 8);
    column.set_margin(32);

    column.add(&mode_combobox);
    column.add(&start_date);
    column.add(&end_date);
    column.add(&book_button);

    window.add(&column);
    window.show_all();

    {
        refresh_end_date_sensitive();
        refresh_button_sensitive();
    }
}

#[derive(Clone, Copy)]
struct Date {
    year: i32,
    month: i32,
    date: i32,
}

impl Date {
    fn to_digital(self) -> i32 {
        let Date {
            year: y,
            month: m,
            date: d,
        } = self;
        ((y * 100) + m * 100) + d
    }

    // impl Ord
    fn cmp(self, other: &Date) -> Ordering {
        self.to_digital().cmp(&other.to_digital())
    }
}

// yyyy-MM-dd. 日付が有効かどうかまでは検査しない
fn parse_date(s: &str) -> Option<Date> {
    if !(s.len() == 10
        && s.chars()
            .all(|c| c.is_ascii_digit() || c == '-' || c == '/'))
    {
        return None;
    }

    let c1 = s.as_bytes()[4];
    let c2 = s.as_bytes()[7];
    if !((c1 == b'-' || c1 == b'/') && c1 == c2) {
        return None;
    }

    let y = s[..4].parse::<i32>().ok()?;
    let m = s[5..7].parse::<i32>().ok()?;
    let d = s[8..].parse::<i32>().ok()?;

    if !((1901 <= y && y <= 2999) && (1 <= m && m <= 12) && (1 <= d && d <= 31)) {
        return None;
    }

    Some(Date {
        year: y,
        month: m,
        date: d,
    })
}

fn main() {
    let application = gtk::Application::new(Some("com.example.flight-booker"), Default::default());

    application.connect_activate(|app| {
        let provider = gtk::CssProvider::new();
        provider.load_from_data(STYLES).expect("load css");

        gtk::StyleContext::add_provider_for_screen(
            &gdk::Screen::default().expect("screen default"),
            &provider,
            gtk::STYLE_PROVIDER_PRIORITY_APPLICATION,
        );

        build_ui(app);
    });

    application.run();
}
