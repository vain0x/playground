#![allow(dead_code)]
#![allow(unused_imports)]

fn recurse<X, Y>(x: X, f: &mut FnMut(X, &mut FnMut(X) -> Y) -> Y) -> Y {
    let fp = f as *mut FnMut(X, &mut FnMut(X) -> Y) -> Y;
    let f1 = unsafe { &mut *fp };
    let f2 = unsafe { &mut *fp };
    f1(x, &mut |x: X| recurse(x, f2))
}

#[cfg(test)]
mod tests {
    use super::*;

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
        let fact = |n| recurse(n, &mut |n, fact| if n <= 1 { 1 } else { n * fact(n - 1) });
        assert_eq!(fact(1), 1);
        assert_eq!(fact(5), 120);
    }

    #[test]
    fn test_dfs() {
        let graph = graph();
        let n = graph.len();

        let mut roots = vec![n; n];
        for u in 0..n {
            recurse(u, &mut |v, go| {
                if roots[v] < n {
                    return;
                }

                roots[v] = u;

                for &w in graph[v].iter() {
                    go(w);
                }
            })
        }

        assert_eq!(roots, vec![0, 0, 0, 0, 4, 4]);
    }
}
