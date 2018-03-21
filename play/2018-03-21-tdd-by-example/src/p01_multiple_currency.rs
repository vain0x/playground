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

use std::convert::Into;

type Currency = &'static str;

struct Bank;

impl Bank {
    fn reduce(&self, source: Expression, currency: Currency) -> i32 {
        6 + 4
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Expression {
    Money(Money),
    Sum(Box<Expression>, Box<Expression>),
}

impl Expression {
    fn plus(self, other: Expression) -> Expression {
        Expression::Sum(Box::new(self), Box::new(other))
    }
}

impl Into<Expression> for Money {
    fn into(self) -> Expression {
        Expression::Money(self)
    }
}

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

    fn plus<R: Into<Expression>>(self, other: R) -> Expression {
        <Money as Into<Expression>>::into(self).plus(other.into())
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
        let expression = dollar(6).plus(dollar(4));
        let reduced = Bank.reduce(expression, "USD");
        assert_eq!(6 + 4, reduced);
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
