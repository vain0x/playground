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
//!     - [x] 5 CHF * 2 = 10 CHF
//!     - [ ] $5 + $5 = $10

type Currency = &'static str;

#[derive(Debug, PartialEq, Eq, Clone)]
struct Money {
    currency: Currency,
    amount: i32,
}

impl Money {
    fn currency(&self) -> Currency {
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

    fn plus(&self, other: Money) -> Money {
        self.with_amount(self.amount() + other.amount())
    }

    fn times(&self, mul: i32) -> Money {
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
    fn test_plus() {
        assert_eq!(dollar(6 + 4), dollar(6).plus(dollar(4)));
    }

    #[test]
    fn test_multiplication() {
        let five = dollar(5);
        assert_eq!(dollar(5 * 2), five.times(2));
        assert_eq!(dollar(5 * 3), five.times(3));

        let five = franc(5);
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
