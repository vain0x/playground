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
struct Money {
    currency: &'static str,
    amount: i32,
}

impl Money {
    fn currency(&self) -> &'static str {
        self.currency
    }

    fn amount(&self) -> i32 {
        self.amount
    }

    fn with_amount(&self, amount: i32) -> Money {
        Money {
            currency: self.currency,
            amount,
        }
    }

    fn times(&mut self, mul: i32) -> Money {
        Money {
            amount: self.amount() * mul,
            ..(*self)
        }
    }
}

fn dollar(amount: i32) -> Money {
    Money {
        currency: "USD",
        amount,
    }
}

fn franc(amount: i32) -> Money {
    Money {
        currency: "CHF",
        amount,
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

        let mut five = franc(5);
        assert_eq!(franc(5 * 2), five.times(2));
        assert_eq!(franc(5 * 3), five.times(3));
    }

    #[test]
    fn test_equality() {
        assert_eq!(dollar(5), dollar(5));
        assert!(dollar(5) != dollar(6));
        assert!(dollar(5) != franc(5));
        assert_eq!(franc(5), franc(5));
        assert!(franc(5) != franc(6));
    }

    #[test]
    fn test_with_amount() {
        assert_eq!(dollar(8), dollar(1).with_amount(8));
        assert_eq!(franc(8), franc(1).with_amount(8));
    }
}
