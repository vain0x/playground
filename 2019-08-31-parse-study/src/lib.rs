pub(crate) mod parse;
pub(crate) mod regexp;

pub fn main() {
    parse::examples::lr1_script::test();
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
