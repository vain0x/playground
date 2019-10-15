#[derive(Default)]
struct State {
    counter: i32,
}

impl State {
    fn new() -> Self {
        State { counter: 0 }
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
