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
//!     - [ ] amount: private
//!     - [x] Dollar side-effects
//!     - [ ] money round

#[derive(Debug, PartialEq, Eq, Clone)]
struct Dollar {
    amount: i32,
}

impl Dollar {
    fn new(amount: i32) -> Dollar {
        Dollar { amount }
    }

    fn times(&mut self, mul: i32) -> Dollar {
        Dollar::new(self.amount * mul)
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn test_multiplication() {
        let mut five = Dollar::new(5);
        let mut product = five.times(2);
        assert_eq!(Dollar::new(5 * 2), product);
        product = five.times(3);
        assert_eq!(Dollar::new(5 * 3), product);
    }

    #[test]
    fn test_equality() {
        assert_eq!(Dollar::new(5), Dollar::new(5));
        assert!(Dollar::new(5) != Dollar::new(6));
    }
}
