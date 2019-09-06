pub(crate) mod parse;
pub(crate) mod regexp;

pub fn main() {
    println!("{:?}", parse::examples::lr0_calc::test());
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
