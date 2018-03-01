use std::env;
use std::*;
use std::io::prelude::*;
use std::io;
use std::fs::{self, DirEntry};
use std::path::Path;

fn go() -> Result<(), io::Error> {
    let library_path = "src";
    let entries = try!(fs::read_dir(library_path));

    for entry in entries {
        let entry = try!(entry);
        let path = entry.file_name();
        println!("{:?}", path);
    }

    return Ok(());
}

fn main() {
    go().unwrap();

    // let mut f = File::open(filename).expect("file not found");

    // let mut contents = String::new();
    // f.read_to_string(&mut contents)
    // .expect("something went wrong reading the file");
    //
    // println!("With text:\n{}", contents);
}
