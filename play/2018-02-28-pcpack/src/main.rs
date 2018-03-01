extern crate toml;

use std::env;
use std::*;
use std::collections::BTreeMap;
use std::io::prelude::*;
use std::io;
use std::fs::{self, DirEntry};
use std::path::Path;

struct Module {
    name: String,
    path: String,
}

#[derive(Debug)]
enum MyError {
    IOError(io::Error),
    ParseError(toml::de::Error),
}

impl convert::From<io::Error> for MyError {
    fn from(error: io::Error) -> MyError {
        return MyError::IOError(error);
    }
}

impl convert::From<toml::de::Error> for MyError {
    fn from(error: toml::de::Error) -> MyError {
        return MyError::ParseError(error);
    }
}

fn read_all_text(path: &Path) -> Result<String, io::Error> {
    let mut file = try!(fs::File::open(path));
    let mut content = String::new();
    file.read_to_string(&mut content);
    return Ok(content);
}

fn go() -> Result<(), MyError> {
    // Move working directory into `workspace`.
    let mut work_dir = try!(env::current_dir());
    {
        work_dir.push("workspace");
        try!(env::set_current_dir(&work_dir.as_path()));
        eprintln!("Current dir = {}", work_dir.display());
    }

    // Read the config file.
    let mut name_to_module: BTreeMap<String, Module> = BTreeMap::new();
    {
        let package_file_path = "pcpack-packages.toml";
        let mut package_file = try!(fs::File::open(package_file_path));
        let mut package_toml = String::new();
        try!(package_file.read_to_string(&mut package_toml));
        eprintln!("toml = {}", package_toml.to_owned());
        let packages: toml::Value = try!(package_toml.parse());
        eprintln!("{:?}", packages);
        /* doc = {
        modules: [
            { name, path },
            ...
        ]
    } */
        let doc = packages.as_table().unwrap();
        let modules = doc.get("modules").unwrap().as_array().unwrap();
        for module in modules {
            let module = module.as_table().unwrap();
            let name = module["name"].as_str().unwrap().to_owned();
            let path = module["path"].as_str().unwrap().to_owned();
            eprintln!("Module '{}' in '{}'.", name, path);
            name_to_module.insert(name.to_owned(), Module { name, path });
        }
    }

    // Read the client file.
    let target_file_name = "program.txt";
    let target_content = try!(read_all_text(Path::new(target_file_name)));
    eprintln!("\n> cat {}\n{}", target_file_name, target_content);

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

#[test]
fn toml_test() {
    let value = "foo = 'bar'".parse::<toml::Value>().unwrap();

    assert_eq!(value["foo"].as_str(), Some("bar"));
}
