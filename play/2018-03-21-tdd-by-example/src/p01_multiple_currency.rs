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
//!     - [ ] rename IntoExpression

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

    /// Converts an expression of money into the specified currency.
    fn reduce<E: IntoExpression>(&self, source: &E, currency: Currency) -> Money {
        source.reduce(&self, currency)
    }
}

#[derive(Debug, PartialEq, Clone)]
struct Money {
    currency: Currency,
    amount: f64,
}

impl Money {
    fn new(amount: f64, currency: Currency) -> Money {
        Money { amount, currency }
    }

    fn currency(&self) -> Currency {
        self.currency
    }

    fn amount(&self) -> f64 {
        self.amount
    }
}

fn dollar(amount: f64) -> Money {
    Money::new(amount, "USD")
}

fn franc(amount: f64) -> Money {
    Money::new(amount, "CHF")
}

#[derive(Debug, PartialEq, Clone)]
struct MoneySum {
    left: Box<Expression>,
    right: Box<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
enum Expression {
    Money(Money),
    Sum(MoneySum),
}

impl Into<Expression> for Money {
    fn into(self) -> Expression {
        Expression::Money(self)
    }
}

impl Into<Expression> for MoneySum {
    fn into(self) -> Expression {
        Expression::Sum(self)
    }
}

trait IntoExpression: Into<Expression> {
    fn plus<R: IntoExpression>(self, other: R) -> Expression {
        Expression::Sum(MoneySum {
            left: Box::new(self.into()),
            right: Box::new(other.into()),
        })
    }

    fn times(&self, mul: f64) -> Self;

    fn reduce_core(&self, bank: &Bank, currency: Currency) -> f64;

    fn reduce(&self, bank: &Bank, currency: Currency) -> Money {
        let amount = self.reduce_core(bank, currency);
        Money::new(amount, currency)
    }
}

impl IntoExpression for Money {
    fn times(&self, mul: f64) -> Money {
        Money::new(mul * self.amount(), self.currency())
    }

    fn reduce_core(&self, bank: &Bank, currency: Currency) -> f64 {
        let rate = bank.rate(self.currency(), currency).unwrap();
        rate * self.amount()
    }
}

impl IntoExpression for MoneySum {
    fn times(&self, mul: f64) -> MoneySum {
        let l = (*self.left).times(mul);
        let r = (*self.right).times(mul);
        MoneySum {
            left: Box::new(l),
            right: Box::new(r),
        }
    }

    fn reduce_core(&self, bank: &Bank, currency: Currency) -> f64 {
        let left = self.left.as_ref().reduce_core(bank, currency);
        let right = self.right.as_ref().reduce_core(bank, currency);
        left + right
    }
}

impl IntoExpression for Expression {
    fn times(&self, mul: f64) -> Expression {
        match self {
            &Expression::Money(ref money) => money.times(mul).into(),
            &Expression::Sum(ref sum) => sum.times(mul).into(),
        }
    }

    fn reduce_core(&self, bank: &Bank, currency: Currency) -> f64 {
        match self {
            &Expression::Money(ref money) => money.reduce_core(bank, currency),
            &Expression::Sum(ref sum) => sum.reduce_core(bank, currency),
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    fn default_bank() -> Bank {
        let mut bank = Bank::new();
        bank.set_rate(dollar(1.0), franc(2.0));
        bank
    }

    #[test]
    fn test_rate() {
        let bank = default_bank();
        assert_eq!(Some(2.0), bank.rate("USD", "CHF"));
    }

    #[test]
    fn test_rate_reverse() {
        let bank = default_bank();
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
        assert_eq!(dollar(5.0), bank.reduce(&five, "USD"));
    }

    #[test]
    fn test_reduce_dollar_to_franc() {
        let bank = default_bank();
        assert_eq!(franc(20.0), bank.reduce(&dollar(10.0), "CHF"));
    }

    #[test]
    #[should_panic]
    fn test_reduce_into_unknown_currency() {
        let bank = Bank::new();
        assert_eq!(Money::new(1.0, "BTC"), bank.reduce(&dollar(9000.0), "BTC"));
    }

    #[test]
    fn test_dollar_plus_dollar() {
        let bank = Bank::new();
        let expression = dollar(6.0).plus(dollar(4.0));
        let reduced = bank.reduce(&expression, "USD");
        assert_eq!(dollar(6.0 + 4.0), reduced);
    }

    #[test]
    fn test_dollar_plus_franc() {
        let bank = default_bank();
        let expression = dollar(5.0).plus(franc(10.0));
        let reduced = bank.reduce(&expression, "USD");
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
    fn test_plus_and_times() {
        let bank = default_bank();
        let sum = dollar(5.0).plus(franc(10.0));
        let mul = sum.times(3.0);
        assert_eq!(dollar(30.0), bank.reduce(&mul, "USD"));
    }

    #[test]
    fn test_equality() {
        assert_eq!(dollar(5.0), dollar(5.0));
        assert!(dollar(5.0) != dollar(6.0));
        assert!(dollar(5.0) != franc(5.0));
        assert_eq!(franc(5.0), franc(5.0));
        assert!(franc(5.0) != franc(6.0));
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
