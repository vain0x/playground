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
//!     - [ ] $5 * 2 = $10
//!     - [ ] amount: private
//!     - [ ] Dollar side-effects
//!     - [ ] money round

struct Dollar {
    pub amount: i32,
}

impl Dollar {
    fn new(amount: i32) -> Dollar {
        Dollar { amount: 10 } // ← 幼稚で視野が狭く、不快な臭いのする実装
    }

    fn times(&mut self, other: i32) {}
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn test_multiplication() {
        let mut five = Dollar::new(5);
        five.times(2);
        assert_eq!(10, five.amount);
    }
}
