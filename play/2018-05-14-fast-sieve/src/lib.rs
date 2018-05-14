#![feature(test)]
#![feature(unboxed_closures)]

extern crate test;

pub fn add_two(a: i32) -> i32 {
    a + 2
}

/// ある非負整数が素数かどうかを定義に従って判定する。
/// O(n^2).
pub fn is_prime_by_def(n: usize) -> bool {
    if n <= 1 {
        return false;
    }
    for k in 2..n {
        if n % k == 0 {
            return false;
        }
    }
    true
}

/// 素数なら true。
/// 定理「合成数 n は √n 以下の素因数を持つ」を利用する。
/// O(n √n)
pub fn is_prime_using_sqrt(n: usize) -> bool {
    if n <= 1 {
        return false;
    }
    let m = (n as f64).sqrt().floor() as usize + 1;
    for k in 2..m {
        if n % k == 0 {
            return false;
        }
    }
    true
}

/// m 未満の非負整数が素数かどうかを O(1) で判定する関数を生成する。
/// エラトステネスのふるいの単純な実装。
/// O(n log(log n))
pub fn sieve_by_def(m: usize) -> impl Fn(usize) -> bool {
    let mut sieve = vec![true; m];
    sieve[0] = false;
    sieve[1] = false;

    for p in 2..m {
        if !sieve[p] {
            continue;
        }

        let mut k = 2 * p;
        while k < m {
            sieve[k] = false;
            k += p;
        }
    }

    move |n: usize| sieve[n]
}

pub fn sieve_compact(m: usize) -> impl Fn(usize) -> bool {
    static REMINDERS: [usize; 8] = [1, 7, 11, 13, 17, 19, 23, 29];

    fn is_prime(n: usize, small: &Vec<bool>, sieve: &Vec<u8>) -> bool {
        if n < 30 {
            return small[n];
        }

        let r = n % 30;
        for i in 0..8 {
            if REMINDERS[i] == r {
                let q = n / 30;
                return (sieve[q - 1] & (1_u8 << i)) != 0;
            }
        }
        false
    }

    let small = (0..30).map(|n| is_prime_using_sqrt(n)).collect::<Vec<_>>();

    let u = (m + 30 - 1) / 30;
    let mut sieve = vec![0xff_u8; u];

    for p in 2..m {
        if !is_prime(p, &small, &sieve) {
            continue;
        }

        let mut k = 2 * p;
        while k < m {
            let q = k / 30;
            let r = k % 30;
            for i in 0..8 {
                if REMINDERS[i] == r {
                    sieve[q - 1] = sieve[q - 1] & !(1_u8 << i);
                    break;
                }
            }
            k += p;
        }
    }

    move |n: usize| is_prime(n, &small, &sieve)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::slice::*;
    use test::Bencher;

    #[test]
    #[ignore]
    fn print_prime_numbers() {
        let m = 1000;
        let prime_numbers = (0..m)
            .filter(|&n| is_prime_by_def(n))
            .map(|n| n.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        assert_eq!(prime_numbers, "");
    }

    #[test]
    fn test_is_prime_using_sqrt() {
        for n in 0..1000 {
            let expected = is_prime_by_def(n);
            let actual = is_prime_using_sqrt(n);
            assert_eq!(actual, expected);
        }
    }

    #[test]
    fn test_sieve_by_def() {
        let m = 1000;
        let sieve = sieve_by_def(m);
        for n in 0..m {
            assert_eq!(sieve(n), is_prime_using_sqrt(n));
        }
    }

    #[test]
    fn test_sieve_compact() {
        let m = 1000;
        let expected_f = sieve_by_def(m);
        let actual_f = sieve_compact(m);
        for n in 0..m {
            assert_eq!(actual_f(n), expected_f(n));
        }
    }

    #[bench]
    fn bench_add_two(b: &mut Bencher) {
        b.iter(|| {
            let n = test::black_box(100);
            (0..n).fold(1, |s, x| if is_prime_using_sqrt(x) { s / 3 } else { s + 1 })
        })
    }

    #[bench]
    fn bench_sieve_by_def(b: &mut Bencher) {
        b.iter(|| {
            let m = test::black_box(10000);
            let is_prime = sieve_by_def(m);
            let mut k = 0_u64;
            for n in 0..m {
                if is_prime(n) {
                    k += 1
                }
            }
            k
        })
    }
}
