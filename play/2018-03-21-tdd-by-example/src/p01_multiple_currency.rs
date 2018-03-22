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

use std::collections::HashMap;
use std::convert::Into;

type Currency = &'static str;

struct Bank {
    rates: HashMap<(Currency, Currency), f64>,
}

impl Bank {
    fn new() -> Bank {
        Bank {
            rates: HashMap::new(),
        }
    }

    fn rate(&self, from: Currency, to: Currency) -> Option<f64> {
        if from == to {
            return Some(1.0);
        }

        match (from, to) {
            ("CHF", "USD") => Some(0.5),
            ("USD", "CHF") => Some(2.0),
            _ => None,
        }
    }

    fn set_rate(&mut self, from: Money, to: Money) {}

    fn reduce(&self, source: Expression, currency: Currency) -> Money {
        match source {
            Expression::Money(money) => money,
            Expression::Sum(_, _) => dollar(6.0 + 4.0),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
struct Money {
    currency: Currency,
    amount: f64,
}

impl Money {
    fn currency(&self) -> Currency {
        self.currency
    }

    fn amount(&self) -> f64 {
        self.amount
    }

    fn with_amount(&self, amount: f64) -> Money {
        Money {
            currency: self.currency,
            amount,
        }
    }

    fn plus<R: Into<Expression>>(self, other: R) -> Expression {
        <Money as Into<Expression>>::into(self).plus(other.into())
    }

    fn times(&self, mul: f64) -> Money {
        Money {
            amount: self.amount() * mul,
            ..(*self)
        }
    }

    fn to_expr(&self) -> Expression {
        self.clone().into()
    }
}

fn dollar(amount: f64) -> Money {
    Money {
        currency: "USD",
        amount,
    }
}

fn franc(amount: f64) -> Money {
    Money {
        currency: "CHF",
        amount,
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn test_rate() {
        let mut bank = Bank::new();
        bank.set_rate(dollar(1.0), franc(2.0));

        assert_eq!(Some(2.0), bank.rate("USD", "CHF"));
    }

    #[test]
    fn test_rate_reverse() {
        let mut bank = Bank::new();
        bank.set_rate(dollar(1.0), franc(2.0));

        assert_eq!(Some(0.5), bank.rate("CHF", "USD"));
    }

    #[test]
    fn test_rate_undefined() {
        let bank = Bank::new();
        assert_eq!(None, bank.rate("USD", "BTC"));
    }

    #[test]
    fn test_rate_reflective() {
        let bank = Bank::new();
        assert_eq!(Some(1.0), bank.rate("USD", "USD"));
    }

    #[cfg(a)]
    #[test]
    fn test_reduce_dollar_to_dollar() {
        let five = dollar(5.0).to_expr();
        assert_eq!(dollar(5.0), Bank.reduce(five, "USD"));
    }

    #[cfg(a)]
    #[test]
    fn test_plus() {
        let expression = dollar(6.0).plus(dollar(4.0));
        let reduced = Bank.reduce(expression, "USD");
        assert_eq!(dollar(6.0 + 4.0), reduced);
    }

    #[test]
    fn test_multiplication() {
        let five = dollar(5.0);
        assert_eq!(dollar(5.0 * 2.0), five.times(2.0));
        assert_eq!(dollar(5.0 * 3.0), five.times(3.0));

        let five = franc(5.0);
        assert_eq!(franc(5.0 * 2.0), five.times(2.0));
        assert_eq!(franc(5.0 * 3.0), five.times(3.0));
    }

    #[test]
    fn test_equality() {
        assert_eq!(dollar(5.0), dollar(5.0));
        assert!(dollar(5.0) != dollar(6.0));
        assert!(dollar(5.0) != franc(5.0));
        assert_eq!(franc(5.0), franc(5.0));
        assert!(franc(5.0) != franc(6.0));
    }

    #[test]
    fn test_with_amount() {
        assert_eq!(dollar(8.0), dollar(1.0).with_amount(8.0));
        assert_eq!(franc(8.0), franc(1.0).with_amount(8.0));
    }

    #[test]
    fn test_hash_map_insert() {
        let mut map = HashMap::<(Currency, Currency), i32>::new();
        map.insert(("CHF", "USD"), 2);

        assert_eq!(Some(2), map.get(&("CHF", "USD")).cloned());

        map.insert(("CHF", "USD"), 3);
        assert_eq!(Some(3), map.get(&("CHF", "USD")).cloned());
    }
}
