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

    #[test]
    #[cfg(xxx)]
    fn test_broken() {
        // https://qiita.com/vain0x/items/90c9580aa34926160ac1#comment-1988da50c4701cc0add8

        let mut b = Some(Box::new(1));
        recurse(true, &mut |flag, f| {
            if flag {
                if let Some(ref b) = b {
                    f(false);
                    println!("b = {}", *b); // SEGV
                }
            } else {
                b.take();
                println!("box droped");
            }
        });
    }

    #[test]
    fn test_closure_is_dropped() {
        let n = 4;
        let mut k = 0;
        struct D<'a>(pub &'a mut i32);
        impl<'a> Drop for D<'a> {
            fn drop(&mut self) {
                *self.0 += 1;
            }
        }

        {
            recurse(0, &mut |i, go| {
                let d = D(&mut k);

                if i >= n {
                    assert_eq!(*d.0, 0);
                    return;
                }

                go(i + 1);
            });
        }

        assert_eq!(k, n + 1);
    }
}
