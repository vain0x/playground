pub(crate) mod compile;
pub(crate) mod syntax;

use std::fs;
use std::path::PathBuf;
use std::rc::Rc;

pub fn run(entry_path: &str) {
    use crate::syntax::*;

    let file = {
        let path = Rc::new(PathBuf::from(entry_path));
        let text = Rc::new(fs::read_to_string(&*path).unwrap());
        SourceFile::new(path, text)
    };

    let node = syntax::functions::parse(file);

    println!("{:?}", node)
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
