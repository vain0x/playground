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
    fn currency(&self) -> &'static str;
    fn amount(&self) -> i32;
}

fn dollar(amount: i32) -> Dollar {
    Dollar {
        currency: "USD",
        amount,
    }
}

fn franc(amount: i32) -> Franc {
    Franc {
        currency: "CHF",
        amount,
    }
}

#[derive(Debug, Eq, Clone)]
struct Dollar {
    currency: &'static str,
    amount: i32,
}

impl Dollar {
    fn times(&mut self, mul: i32) -> Dollar {
        dollar(self.amount() * mul)
    }
}

impl Money for Dollar {
    fn currency(&self) -> &'static str {
        self.currency
    }

    fn amount(&self) -> i32 {
        self.amount
    }
}

impl<T: Money> PartialEq<T> for Dollar {
    fn eq(&self, other: &T) -> bool {
        self.currency() == other.currency() && self.amount() == other.amount()
    }
}

#[derive(Debug, Eq, Clone)]
struct Franc {
    currency: &'static str,
    amount: i32,
}

impl Franc {
    fn times(&mut self, mul: i32) -> Franc {
        franc(self.amount() * mul)
    }
}

impl Money for Franc {
    fn currency(&self) -> &'static str {
        self.currency
    }

    fn amount(&self) -> i32 {
        self.amount
    }
}

impl<T: Money> PartialEq<T> for Franc {
    fn eq(&self, other: &T) -> bool {
        self.currency() == other.currency() && self.amount() == other.amount()
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn test_multiplication() {
        let mut five = dollar(5);
        assert_eq!(dollar(5 * 2), five.times(2));
        assert_eq!(dollar(5 * 3), five.times(3));
    }

    #[test]
    fn test_equality() {
        assert_eq!(dollar(5), dollar(5));
        assert!(dollar(5) != dollar(6));
        assert!(dollar(5) != franc(5));
    }

    #[test]
    fn test_franc_multiplication() {
        let mut five = franc(5);
        assert_eq!(franc(5 * 2), five.times(2));
        assert_eq!(franc(5 * 3), five.times(3));
    }

    #[test]
    fn test_franc_equality() {
        assert_eq!(franc(5), franc(5));
        assert!(franc(5) != franc(6));
    }
}
