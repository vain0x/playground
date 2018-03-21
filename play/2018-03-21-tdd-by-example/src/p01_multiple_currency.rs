//! # Reading "TDD by Example" Part I.
//!
//! Mantra:
//!    - Red, Greep, Refatoring, Loop.
//!
//! Rule:
//!     - Write new code only when automated tests failed.
//!     - Remove duplication.
//!
//! TODO:
//!     - [ ] $5 + 10 CHF = $10
//!     - [x] $5 * 2 = $10
//!     - [x] amount: private
//!     - [x] Dollar side-effects
//!     - [ ] money round
//!     - [ ] 5 CHF * 2 = 10 CHF

use std::fmt::Debug;

trait Money: Debug + PartialEq + Clone {
    fn tag(&self) -> &'static str;
    fn amount(&self) -> i32;
}

#[derive(Debug, Eq, Clone)]
struct Dollar {
    tag: &'static str,
    amount: i32,
}

impl Dollar {
    fn new(amount: i32) -> Dollar {
        Dollar { tag: "USD", amount }
    }

    fn times(&mut self, mul: i32) -> Dollar {
        Dollar::new(self.amount() * mul)
    }
}

impl Money for Dollar {
    fn tag(&self) -> &'static str {
        self.tag
    }

    fn amount(&self) -> i32 {
        self.amount
    }
}

impl<T: Money> PartialEq<T> for Dollar {
    fn eq(&self, other: &T) -> bool {
        self.tag() == other.tag() && self.amount() == other.amount()
    }
}

#[derive(Debug, Eq, Clone)]
struct Franc {
    tag: &'static str,
    amount: i32,
}

impl Franc {
    fn new(amount: i32) -> Franc {
        Franc { tag: "CHF", amount }
    }

    fn times(&mut self, mul: i32) -> Franc {
        Franc::new(self.amount() * mul)
    }
}

impl Money for Franc {
    fn tag(&self) -> &'static str {
        self.tag
    }

    fn amount(&self) -> i32 {
        self.amount
    }
}

impl<T: Money> PartialEq<T> for Franc {
    fn eq(&self, other: &T) -> bool {
        self.tag() == other.tag() && self.amount() == other.amount()
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn test_multiplication() {
        let mut five = Dollar::new(5);
        assert_eq!(Dollar::new(5 * 2), five.times(2));
        assert_eq!(Dollar::new(5 * 3), five.times(3));
    }

    #[test]
    fn test_equality() {
        assert_eq!(Dollar::new(5), Dollar::new(5));
        assert!(Dollar::new(5) != Dollar::new(6));
        assert!(Dollar::new(5) != Franc::new(5));
    }

    #[test]
    fn test_franc_multiplication() {
        let mut five = Franc::new(5);
        assert_eq!(Franc::new(5 * 2), five.times(2));
        assert_eq!(Franc::new(5 * 3), five.times(3));
    }

    #[test]
    fn test_franc_equality() {
        assert_eq!(Franc::new(5), Franc::new(5));
        assert!(Franc::new(5) != Franc::new(6));
    }
}
