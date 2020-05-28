#![allow(dead_code)]
#![allow(unused_macros)]
#![allow(unused_imports)]

fn recurse<X, Y>(x: X, f: &Fn(X, &Fn(X) -> Y) -> Y) -> Y {
    f(x, &|x: X| recurse(x, &f))
}

macro_rules! memo {
    (| $f:ident, $($p:ident $(: $t:ty)*),* | $body:expr) => {{
        use std;
        let memo = std::cell::RefCell::new(std::collections::HashMap::new());

        move |$($p $(: $t)*),*| {
            recurse(
                #[allow(unused_parens)]
                { ($($p),*) },
                &|$($p $(: $t)*),*, $f| {
                    let args = ($($p),*).clone();
                    if let Some(&y) = memo.borrow().get(&args) {
                        return y;
                    }
                    let y = $body;
                    memo.borrow_mut().insert(args, y.clone());
                    y
                }
            )
        }
    }};
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::RefCell;

    fn graph() -> Vec<Vec<usize>> {
        //
        //   0 -- 1
        //   | \
        //   |  \
        //   2 -- 3    4--5
        //
        vec![
            vec![1, 2, 3],
            vec![0],
            vec![0, 3],
            vec![0, 2],
            vec![5],
            vec![4],
        ]
    }

    #[test]
    fn test_fact() {
        let fact = |n| recurse(n, &|n, fact| if n <= 1 { 1_i64 } else { n * fact(n - 1) });
        assert_eq!(fact(1), 1);
        assert_eq!(fact(5), 120);
    }

    #[test]
    fn test_dfs() {
        let graph = graph();
        let n = graph.len();

        let roots = RefCell::new(vec![n; n]);
        for u in 0..n {
            recurse(u, &|v, go| {
                if roots.borrow()[v] < n {
                    return;
                }

                roots.borrow_mut()[v] = u;

                for &w in graph[v].iter() {
                    go(w);
                }
            })
        }

        assert_eq!(&*roots.borrow(), &[0, 0, 0, 0, 4, 4]);
    }

    #[test]
    fn test_memoized_fib() {
        let fib = memo!(|fib, n: i32| if n <= 1 {
            1_i64
        } else {
            fib(n - 1) + fib(n - 2)
        });
        assert_eq!(fib(5), 8);
        assert_eq!(fib(20), 10946);
    }
}
