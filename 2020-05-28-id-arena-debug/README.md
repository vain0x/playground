# ID Arena + Debug

Conceptually same as [id-arena](https://github.com/fitzgen/id-arena) but `Arena::Id` from this crate implements `Debug` trait to print the *contents of the object* rather than just an integer ID.

## Illustration

```rust
// Just for illustration. Not works actually.

    let mut names = Arena::new();

    // `alice` is ID of the string.
    let alice = names.insert("alice");

    // `alice` prints a string in debug format.
    assert_eq!(format!("{:?}", alice), r##""alice""##);
```

## Mechanism

```rust
// Just for illustration. Not same as the implementation.

struct Arena<T> {
    inner: Vec<T>,

    // Debug-only field.
    // The pointer points to the buffer of `inner`.
    debug_info: Box<*const [T]>,
}

// sizeof(Id<T>) is 4 (`u32`) in release mode.
struct Id<T> {
    id: u32,

    // Debug-only field.
    // Points to the content of `Arena::debug_info` box.
    ptr: *const *const [T],
}

impl Debug for Id {
    fn fmt(&self, f) {
        // Print the content if debug mode.
        write!(f, "{:?}", (*self.ptr)[id])

        // Print the ID if release mode (or compile error?)
        write!(f, "{}", self.id)
    }
}
```

Complete documentation isn't available yet. See implementations for details.
