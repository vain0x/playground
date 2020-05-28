mod vec_arena_debug;
mod vec_arena_id;

use self::{
    vec_arena_debug::{ArenaDebug, DefaultArenaDebug, NullArenaDebug},
    vec_arena_id::Id,
};
use super::arena::Arena;
use std::{
    fmt,
    ops::{Deref, Index, IndexMut},
};

pub(crate) type DebugVecArena<T> = VecArena<T, DefaultArenaDebug<T>>;

pub(crate) type ReleaseVecArena<T> = VecArena<T, NullArenaDebug<T>>;

pub(crate) struct VecArena<T, D> {
    data: Vec<T>,
    debug: D,
}

impl<T, D: ArenaDebug<Item = T>> VecArena<T, D> {
    pub(crate) fn new() -> Self {
        VecArena::with_capacity(0)
    }

    pub(crate) fn with_capacity(capacity: usize) -> Self {
        let data = Vec::with_capacity(capacity);
        let debug = D::new();
        Self { data, debug }
    }

    pub(crate) fn len(&self) -> usize {
        self.data.len()
    }

    pub(crate) fn as_slice(&self) -> &[T] {
        self.data.as_slice()
    }

    pub(crate) fn get(&self, id: Id<T, D>) -> &T {
        &self.data[id.get() as usize]
    }

    pub(crate) fn get_mut(&mut self, id: Id<T, D>) -> &mut T {
        &mut self.data[id.get() as usize]
    }

    pub(crate) fn insert(&mut self, value: T) -> Id<T, D> {
        let index = self.data.len();
        self.data.push(value);
        self.debug.did_create(index as u32, self);

        let mut id = Id::new(index as u32, D::IdDebug::default());
        self.debug.will_share(id.get(), &mut id.debug);
        id
    }
}

impl<T, D> fmt::Debug for VecArena<T, D>
where
    Vec<T>: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Vec<T> as fmt::Debug>::fmt(&self.data, f)
    }
}

impl<T, D: ArenaDebug<Item = T>> Default for VecArena<T, D> {
    fn default() -> Self {
        VecArena::with_capacity(0)
    }
}

impl<T, D> Deref for VecArena<T, D> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        self.data.as_slice()
    }
}

impl<T, D: ArenaDebug<Item = T>> Index<Id<T, D>> for VecArena<T, D> {
    type Output = T;

    fn index(&self, index: Id<T, D>) -> &T {
        &self.data[index.get() as usize]
    }
}

impl<T, D: ArenaDebug<Item = T>> IndexMut<Id<T, D>> for VecArena<T, D> {
    fn index_mut(&mut self, index: Id<T, D>) -> &mut T {
        &mut self.data[index.get() as usize]
    }
}

impl<T> Arena<T> for VecArena<T, DefaultArenaDebug<T>> {
    type Id = Id<T, DefaultArenaDebug<T>>;
}
