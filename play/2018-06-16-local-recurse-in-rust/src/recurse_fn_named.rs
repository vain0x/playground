#![allow(unused_imports)]

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    #[test]
    fn test_memoized_fn_fib() {
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
        let mut fib = move |n: i32| mfib(n, &mut memo);

        assert_eq!(fib(5), 8);
        assert_eq!(fib(20), 10946);
    }

    #[test]
    fn test_memoized_method_fib() {
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

        let mut fib = move |n: i32| fib.call(n);

        assert_eq!(fib(5), 8);
        assert_eq!(fib(20), 10946);
    }

    #[test]
    fn test_dp_fib() {
        let mut dp = vec![1, 1];
        let mut fib = move |n: usize| {
            for i in dp.len()..n + 1 {
                let fib_i = dp[i - 1] + dp[i - 2];
                dp.push(fib_i);
            }
            dp[n]
        };

        assert_eq!(fib(5), 8);
        assert_eq!(fib(20), 10946);
    }
}
