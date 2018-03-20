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

#[derive(Debug, PartialEq, Eq, Clone)]
struct Franc {
    amount: i32,
}

impl Franc {
    fn new(amount: i32) -> Franc {
        Franc { amount }
    }

    fn times(&mut self, mul: i32) -> Franc {
        Franc::new(self.amount * mul)
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
