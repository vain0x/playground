use super::{
    vec_arena_debug::{ArenaDebug, DefaultArenaDebug, NullArenaDebug},
    VecArena,
};
use std::{
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    marker::PhantomData,
    ops::Index,
};

pub(crate) struct Id<T, D: ArenaDebug> {
    id: u32,
    pub(super) debug: D::IdDebug,

    #[allow(dead_code)]
    phantom: Option<fn() -> PhantomData<T>>,
}

impl<T, D: ArenaDebug<Item = T>> Id<T, D> {
    pub(crate) fn new(id: u32, debug: D::IdDebug) -> Self {
        Self {
            id,
            debug,
            phantom: None,
        }
    }

    pub(crate) fn get(self) -> u32 {
        self.id
    }

    pub(crate) fn of(self, arena: &VecArena<T, D>) -> &T {
        arena.get(self)
    }
}

impl<T, D: ArenaDebug> Clone for Id<T, D> {
    fn clone(&self) -> Self {
        Self {
            id: self.id,
            debug: self.debug,
            phantom: None,
        }
    }
}

impl<T, D: ArenaDebug> Copy for Id<T, D> {}

impl<T, D: ArenaDebug> PartialEq for Id<T, D> {
    fn eq(&self, other: &Self) -> bool {
        entity_is_compatible_with(self, other);

        self.id == other.id
    }
}

impl<T, D: ArenaDebug> Eq for Id<T, D> {}

impl<T, D: ArenaDebug> PartialOrd for Id<T, D> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        entity_is_compatible_with(self, other);

        self.id.partial_cmp(&other.id)
    }
}

impl<T, D: ArenaDebug> Ord for Id<T, D> {
    fn cmp(&self, other: &Self) -> Ordering {
        entity_is_compatible_with(self, other);

        self.id.cmp(&other.id)
    }
}

impl<T, D: ArenaDebug> Hash for Id<T, D> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl<T: fmt::Debug> fmt::Debug for Id<T, NullArenaDebug<T>> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#{}", self.id)
    }
}

impl<T: fmt::Debug> fmt::Debug for Id<T, DefaultArenaDebug<T>> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(value) = self.debug.borrow(self.id) {
            fmt::Debug::fmt(value, f)
        } else {
            write!(f, "#{}", self.id)
        }
    }
}

fn entity_is_compatible_with<T, D: ArenaDebug>(it: &Id<T, D>, other: &Id<T, D>) {
    todo!()
    // it.debug.is_same_as(&other.debug);
}
