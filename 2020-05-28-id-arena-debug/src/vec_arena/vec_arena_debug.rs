use super::VecArena;
use std::{
    cell::{Cell, RefCell},
    marker::PhantomData,
    mem::replace,
};

pub(crate) trait ArenaDebug: Sized {
    type Item;

    type ItemDebug;

    type IdDebug: Copy + Default;

    fn new() -> Self;

    fn did_create(&self, id: u32, arena: &VecArena<Self::Item, Self>);

    fn will_share(&self, id: u32, id_debug: &mut Self::IdDebug);
}

pub(crate) struct NullArenaDebug<T>(PhantomData<T>);

impl<T> ArenaDebug for NullArenaDebug<T> {
    type Item = T;

    type IdDebug = ();

    type ItemDebug = ();

    fn new() -> Self {
        NullArenaDebug(PhantomData)
    }

    fn did_create(&self, _id: u32, _arena: &VecArena<Self::Item, Self>) {}

    fn will_share(&self, _id: u32, _id_debug: &mut Self::IdDebug) {}
}

struct DebugData<T> {
    data: *const [T],
    item_debugs: Vec<DefaultItemDebug>,
}

pub(crate) struct DefaultArenaDebug<T>(RefCell<Box<DebugData<T>>>);

impl<T> DefaultArenaDebug<T> {
    pub(crate) fn get_unchecked(&self, index: u32) -> Option<&T> {
        unsafe { (*self.0.borrow().data).get(index as usize) }
    }
}

impl<T> ArenaDebug for DefaultArenaDebug<T> {
    type Item = T;

    type ItemDebug = DefaultItemDebug;

    type IdDebug = DefaultIdDebug<T>;

    fn new() -> Self {
        Self(RefCell::new(Box::new(DebugData {
            data: &[] as *const [T],
            item_debugs: Vec::default(),
        })))
    }

    fn did_create(&self, id: u32, arena: &VecArena<T, Self>) {
        assert_eq!(id as usize, self.0.borrow().item_debugs.len());

        let mut data = self.0.borrow_mut();
        data.data = arena.data.as_slice() as *const [T];
        data.item_debugs.push(DefaultItemDebug::new());
    }

    fn will_share(&self, _id: u32, id_debug: &mut Self::IdDebug) {
        id_debug.ptr = Box::as_ref(&self.0.borrow()) as *const _;
    }
}

pub(crate) struct DefaultIdDebug<T> {
    ptr: *const DebugData<T>,
}

impl<T> DefaultIdDebug<T> {
    fn get_item_debug(&self, id: u32) -> &DefaultItemDebug {
        let debug = &unsafe { &*self.ptr };
        &debug.item_debugs[id as usize]
    }

    pub(crate) fn get(&self, id: u32) -> &T {
        let data = unsafe { &*(*self.ptr).data };
        &data[id as usize]
    }

    pub(crate) fn enter(&self, id: u32) -> bool {
        self.get_item_debug(id).enter()
    }

    pub(crate) fn leave(&self, id: u32) -> bool {
        self.get_item_debug(id).leave()
    }
}

impl<T> Clone for DefaultIdDebug<T> {
    fn clone(&self) -> Self {
        Self { ptr: self.ptr }
    }
}

impl<T> Copy for DefaultIdDebug<T> {}

impl<T> Default for DefaultIdDebug<T> {
    fn default() -> Self {
        Self {
            ptr: std::ptr::null(),
        }
    }
}

pub(crate) struct DefaultItemDebug {
    visiting: Cell<bool>,
}

impl DefaultItemDebug {
    pub(crate) fn new() -> Self {
        Self {
            visiting: Cell::new(false),
        }
    }

    pub(crate) fn enter(&self) -> bool {
        !self.visiting.replace(true)
    }

    pub(crate) fn leave(&self) -> bool {
        self.visiting.replace(false)
    }
}
