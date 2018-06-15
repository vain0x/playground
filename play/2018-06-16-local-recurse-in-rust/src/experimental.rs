#![allow(dead_code)]
#![allow(unused_imports)]

use std;
use std::cell::*;
use std::collections::*;
use std::mem::*;
use std::ptr::*;
use std::rc::*;

pub trait FnMutRec<X, Y> {
    fn call(&mut self, x: X) -> Y;
}

pub struct ClosureFnMutRec<F>(F);

impl<X, Y, F> FnMutRec<X, Y> for ClosureFnMutRec<F>
where
    F: FnMut(&mut FnMut(X) -> Y, X) -> Y,
{
    fn call(&mut self, x: X) -> Y {
        // Duplicate mutable reference.
        let f = unsafe { &mut *(&mut self.0 as *mut F) };

        f(&mut |x: X| self.call(x), x)
    }
}

// FIXME: Replace the result type with impl FnMut in Rust 1.26+.
pub fn recursive<X, Y, F>(f: F) -> ClosureFnMutRec<F>
where
    F: FnMut(&mut FnMut(X) -> Y, X) -> Y,
{
    ClosureFnMutRec(f)
}

pub fn recurse_with<X, Y, F>(x: X, f: F) -> Y
where
    F: FnMut(&mut FnMut(X) -> Y, X) -> Y,
{
    ClosureFnMutRec(f).call(x)
}

pub struct Y<F>(pub F);

impl<F> Y<F> {
    pub fn apply<In, Out>(&self, value: In) -> Out
    where
        F: Fn(&Fn(In) -> Out, In) -> Out,
    {
        fn y<In, Out>(func: &Fn(&Fn(In) -> Out, In) -> Out, value: In) -> Out {
            func(&|v: In| -> Out { y(func, v) }, value)
        }
        y(&self.0, value)
    }
}

mod tests {
    use super::*;

    #[test]
    fn test_fib() {
        let mut fib1 = {
            fn mfib(n: i32, memo: &mut HashMap<i32, i64>) -> i64 {
                if let Some(&y) = memo.get(&n) {
                    return y;
                }

                let y = if n <= 1 {
                    1
                } else {
                    mfib(n - 2, memo) + mfib(n - 1, memo)
                };

                memo.insert(n, y);
                y
            }

            let mut memo = HashMap::new();
            move |n: i32| mfib(n, &mut memo)
        };

        let mut fib2 = {
            struct Fib {
                memo: HashMap<i32, i64>,
            };
            impl Fib {
                fn call(&mut self, n: i32) -> i64 {
                    if let Some(&y) = self.memo.get(&n) {
                        return y;
                    }

                    let y = if n <= 1 {
                        1
                    } else {
                        self.call(n - 2) + self.call(n - 1)
                    };

                    self.memo.insert(n, y);
                    y
                }
            }
            let mut fib = Fib {
                memo: HashMap::new(),
            };
            move |n: i32| fib.call(n)
        };

        let mut fib = {
            let mut dp = vec![1, 1];
            move |n: usize| {
                for i in dp.len()..n + 1 {
                    let fib_i = dp[i - 1] + dp[i - 2];
                    dp.push(fib_i);
                }
                dp[n]
            }
        };

        assert_eq!(fib(0), 1);
        assert_eq!(fib(1), 1);
        assert_eq!(fib(2), 2);
        assert_eq!(fib(5), 8);
        assert_eq!(fib(10), 89);

        assert_eq!(fib1(0), 1);
        assert_eq!(fib2(1), 1);
    }

    #[test]
    #[cfg(no_compile)]
    fn test_fib_macro() {
        let mut memo = HashMap::new();

        rec!(fib, [memo], |n: i32| {
            if let Some(&y) = memo.get(&n) {
                return y;
            }

            let y = if n <= 1 {
                1
            } else {
                mfib(n - 2, memo) + mfib(n - 1, memo)
            };

            memo.insert(n, y);
            y
        });

        assert_eq!(fib(5), 8);
    }

    #[test]
    #[cfg(no_compile)]
    fn test_recurser() {
        let memo = RefCell::new(HashMap::new());
        let fib = Y(move |fib: &Fn(i32) -> i64, n: i32| {
            if let Some(&y) = memo.borrow().get(&n) {
                return y;
            }

            let y = if n <= 1 { 1 } else { fib(n - 2) + fib(n - 1) };

            memo.borrow_mut().insert(n, y);
            y
        });

        assert_eq!(fib.apply(5), 8);
    }

    #[test]
    #[cfg(no_compile)]
    fn test_fnrec() {
        let fib = |n: i32| {
            let memo = RefCell::new(HashMap::new());
            recurse(n, {
                move |fib, n| {
                    if let Some(&y) = memo.borrow().get(&n) {
                        return y;
                    }

                    let y = if n <= 1 {
                        1_i64
                    } else {
                        fib(n - 2) + fib(n - 1)
                    };

                    memo.borrow_mut().insert(n, y);
                    y
                }
            })
        };

        assert_eq!(fib(5), 8);
    }

}
