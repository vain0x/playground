use std::io::{self, BufWriter, Write};
use std::net::TcpListener;
use std::sync::{Arc, Mutex};
use std::thread;

fn send_http_response<W: Write>(w: &mut W, id: i64) -> io::Result<()> {
    let mut w = BufWriter::new(w);

    let text = format!(
        "<html><body><string>Hello, world! ({})</strong></body></html>",
        id
    );
    write!(
        w,
        "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=utf-8\r\nContent-Length: {}\r\n\r\n",
        text.len()
    )?;
    w.write_all(text.as_bytes())
}

#[derive(Default)]
struct State {
    counter: i32,
    cancel: Arc<Mutex<bool>>,
    thread: Option<thread::JoinHandle<()>>,
}

impl State {
    fn new() -> Self {
        State::default()
    }

    fn start_tcp_listener(&mut self) {
        let cancel = self.cancel.clone();

        let t = thread::spawn(move || match TcpListener::bind("0.0.0.0:8080") {
            Err(err) => {
                eprintln!("tcp bind error {:?}", err);
            }
            Ok(listener) => {
                eprintln!("tcp listening...");

                listener
                    .set_nonblocking(true)
                    .expect("can't be non-blocking");

                let mut id = 0;

                for mut result in listener.incoming() {
                    match result {
                        Ok(ref mut stream) => {
                            println!("new connection!");
                            id += 1;

                            if let Err(err) = send_http_response(stream, id) {
                                eprintln!("tcp write error {:?}", err);
                                // break;
                            }
                        }
                        Err(ref err) if err.kind() == io::ErrorKind::WouldBlock => {
                            if cancel.lock().ok().map_or(true, |is_canceled| *is_canceled) {
                                break;
                            }
                            std::thread::sleep(std::time::Duration::from_millis(16));
                            continue;
                        }
                        Err(ref err) => {
                            eprintln!("tcp accept error {:?}", err);
                            break;
                        }
                    }
                }

                eprintln!("tcp gracefully exited");
            }
        });
        self.thread = Some(t);
    }
}

impl Drop for State {
    fn drop(&mut self) {
        if let Some(mut lock) = self.cancel.lock().ok() {
            *lock = true;
        }

        if let Some(join_handle) = self.thread.take() {
            join_handle.join().ok();
        }
    }
}

mod global {
    use super::State;
    use std::cell::RefCell;
    use std::sync::Mutex;

    static mut STATE: Option<Mutex<RefCell<Option<State>>>> = None;

    pub(crate) fn initialize() {
        unsafe {
            if STATE.is_none() {
                STATE = Some(Mutex::new(RefCell::new(Some(State::new()))));
            }
        }
    }

    pub(crate) fn drop() {
        let mutex = unsafe {
            match STATE.as_ref() {
                Some(mutex) => mutex,
                None => return,
            }
        };
        let locked_cell = mutex.lock().unwrap();
        let mut borrowed_state = locked_cell.borrow_mut();
        borrowed_state.take();
    }

    pub(crate) fn use_mut<T, F: FnOnce(&mut State) -> T>(body: F) -> T {
        let mutex = unsafe { STATE.as_ref().unwrap() };

        let locked_cell = mutex.lock().unwrap();
        let mut borrowed_state = locked_cell.borrow_mut();

        match borrowed_state.as_mut() {
            None => unreachable!("Already dropped"),
            Some(state) => body(state),
        }
    }
}

#[no_mangle]
pub extern "C" fn rlib_initialize() {
    global::initialize();
}

#[no_mangle]
pub extern "C" fn rlib_drop() {
    global::drop();
}

#[no_mangle]
pub extern "C" fn rlib_start() {
    global::use_mut(|state| state.start_tcp_listener());
}

#[no_mangle]
pub extern "C" fn rlib_add(x: i32) -> i32 {
    global::use_mut(|state| {
        state.counter += x;
        state.counter
    })
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
