use std::ops::Index;
use std::rc::Rc;

pub struct Seg<T> {
    parent: Option<Rc<Seg<T>>>,
    items: Vec<T>,
}

impl<T> Seg<T> {
    fn new() -> Seg<T> {
        Seg {
            parent: None,
            items: Vec::new(),
        }
    }

    fn cons(item: T, parent: Rc<Seg<T>>) -> Seg<T> {
        let capacity = (*parent).items.capacity() * 2;
        let mut items = Vec::with_capacity(capacity);
        Seg {
            parent: Some(parent),
            items,
        }
    }
}

pub struct VList<T> {
    seg: Seg<T>,

    /// Number of leading elements to count in `seg.items`.
    take: usize,
}

impl<T> VList<T> {
    pub fn new() -> VList<T> {
        VList {
            seg: Seg::new(),
            take: 0,
        }
    }

    pub fn with_capacity(capacity: usize) -> VList<T> {
        panic!("not impl")
    }

    pub fn is_empty(&self) -> bool {
        self.seg.parent.is_none() && self.take == 0
    }

    pub fn len(&self) -> usize {
        fn len<T>(seg: &Seg<T>) -> usize {
            match seg.parent {
                None => 0,
                Some(ref parent) => len(parent),
            }
        }
        len(&self.seg)
    }

    pub fn push_front(mut self, item: T) -> VList<T> {
        if self.is_empty() {
            return VList {
                seg: Seg::cons(item, Rc::new(self.seg)),
                take: 1,
            };
        }

        if self.seg.items.len() != self.take {
            panic!("?")
        }

        if self.seg.items.len() + 1 <= self.seg.items.capacity() {
            self.seg.items.push(item);
            self.take += 1;
            return self;
        }

        assert_eq!(self.seg.items.len(), self.seg.items.capacity());

        return VList {
            seg: Seg::cons(item, Rc::new(self.seg)),
            take: 1,
        };
    }

    pub fn pop_front(&self) -> VList<T> {
        if self.is_empty() {
            return VList::new();
        }

        if self.take == 1 {
            return match self.seg.parent {
                None => VList::new(),
                Some(ref parent) => VList {
                    seg: parent,
                    take: parent.items.len(),
                },
            };
        }

        return VList {
            seg: self.seg,
            take: self.take - 1,
        };
    }
}

impl<T> Index<usize> for VList<T> {
    type Output = T;

    fn index(&self, i: usize) -> &T {}
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
