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
//!     - [ ] money round
//!     - [ ] reduce and undefined currency rate

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

        if let Some(&r) = self.rates.get(&(from, to)) {
            return Some(r);
        }

        if let Some(&r) = self.rates.get(&(to, from)) {
            return Some(1.0 / r);
        }

        None
    }

    fn set_rate(&mut self, from: Money, to: Money) {
        if from.currency() == to.currency() {
            if from.amount() != to.amount() {
                panic!("Inconsistent.");
            }
            return;
        }

        self.rates.insert(
            (from.currency(), to.currency()),
            to.amount() / from.amount(),
        );
    }

    /// Calculates amount of money that the specified expression represents, converting into the specified currency.
    fn reduce<E: IntoExpression>(&self, source: E, currency: Currency) -> Money {
        fn reduce_core(bank: &Bank, source: Expression, currency: Currency) -> f64 {
            match source {
                Expression::Money(money) => {
                    if money.currency() == currency {
                        return money.amount();
                    }

                    let rate = bank.rate(money.currency(), currency).unwrap();
                    rate * money.amount()
                }
                Expression::Sum(left, right) => {
                    let left = reduce_core(bank, *left, currency);
                    let right = reduce_core(bank, *right, currency);
                    left + right
                }
            }
        }

        let amount = reduce_core(self, source.into(), currency);
        Money { amount, currency }
    }
}

#[derive(Debug, PartialEq, Clone)]
enum Expression {
    Money(Money),
    Sum(Box<Expression>, Box<Expression>),
}

impl Into<Expression> for Money {
    fn into(self) -> Expression {
        Expression::Money(self)
    }
}

trait IntoExpression: Into<Expression> {
    fn into_expr(self) -> Expression {
        self.into()
    }

    fn to_expr(&self) -> Expression
    where
        Self: Clone,
    {
        self.clone().into_expr()
    }

    fn plus<R: Into<Expression>>(self, other: R) -> Expression {
        Expression::Sum(Box::new(self.into_expr()), Box::new(other.into_expr()))
    }
}

impl<E> IntoExpression for E
where
    E: Into<Expression>,
{
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

    fn times(&self, mul: f64) -> Money {
        Money {
            amount: self.amount() * mul,
            ..(*self)
        }
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

    #[test]
    #[should_panic]
    fn test_set_rate_inconsistent() {
        let mut bank = Bank::new();

        // End the USA economics!!
        bank.set_rate(dollar(1.0), dollar(2.0));
    }

    #[test]
    fn test_reduce_dollar_to_dollar() {
        let bank = Bank::new();
        let five = dollar(5.0);
        assert_eq!(dollar(5.0), bank.reduce(five, "USD"));
    }

    #[test]
    fn test_reduce_dollar_to_franc() {
        let mut bank = Bank::new();
        bank.set_rate(dollar(1.0), franc(2.0));
        assert_eq!(franc(20.0), bank.reduce(dollar(10.0), "CHF"));
    }

    #[test]
    fn test_dollar_plus_dollar() {
        let bank = Bank::new();
        let expression = dollar(6.0).plus(dollar(4.0));
        let reduced = bank.reduce(expression, "USD");
        assert_eq!(dollar(6.0 + 4.0), reduced);
    }

    #[test]
    fn test_dollar_plus_franc() {
        let mut bank = Bank::new();
        bank.set_rate(dollar(1.0), franc(2.0));

        let expression = dollar(5.0).plus(franc(10.0));
        let reduced = bank.reduce(expression, "USD");
        assert_eq!(dollar(5.0 + 10.0 / 2.0), reduced);
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

    // Learning tests.

    #[test]
    fn test_hash_map_insert() {
        let mut map = HashMap::<(Currency, Currency), i32>::new();
        map.insert(("CHF", "USD"), 2);

        assert_eq!(Some(2), map.get(&("CHF", "USD")).cloned());

        map.insert(("CHF", "USD"), 3);
        assert_eq!(Some(3), map.get(&("CHF", "USD")).cloned());
    }
}
