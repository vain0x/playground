extern crate regex;
extern crate toml;

use std::env;
use std::*;
use std::ops::*;
use std::io::Write as IOWrite;
use std::fmt::Write as FmtWrite;
use std::collections::BTreeMap;
use std::collections::HashSet;
use std::io::prelude::*;
use std::io;
use std::fs::{self, DirEntry};
use std::path::Path;
use std::path::PathBuf;
use regex::Regex;
use std::fs::File;
use std::io::prelude::*;
use std::iter::*;

struct Module {
    name: String,
    path: PathBuf,
    parents: Vec<String>,
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
    try!(file.read_to_string(&mut content));
    return Ok(content);
}

fn write_all_text(path: &Path, content: &str) -> Result<(), io::Error> {
    let mut file = try!(fs::File::create(path));
    try!(file.write_all(content.as_bytes()));
    return Ok(());
}

fn split_with_slot(source: &str) -> Option<(String, String)> {
    let begin_pattern = Regex::new("(?m)^ *(?://|#|--) *pcpack *slot:begin.*$").unwrap();
    let end_pattern = Regex::new("(?m)^ *(?://|#|--) *pcpack *slot:end").unwrap();

    let begin_match = begin_pattern.find(source);
    let end_match = end_pattern.find(source);
    match (begin_match, end_match) {
        (Some(ref begin_match), Some(ref end_match)) => {
            let first = begin_match.end();
            let end = end_match.start();
            if first > end {
                eprintln!(
                    "slot:begin (at {}) must be followed by slot:end (at {}).",
                    first, end
                );
                return None;
            }

            let first_half = source[0..first].to_owned();
            let second_half = source[end..source.len()].to_owned();
            return Some((first_half, second_half));
        }
        (_, _) => {
            return None;
        }
    }
}

fn find_required_module_names(source: &str) -> Vec<String> {
    let use_pattern = Regex::new("(?://|#|--) *pcpack *use ((?:,? *[\\w\\d_-]+)*)").unwrap();

    let captures = use_pattern.captures_iter(source);

    return captures
        .flat_map(|cap| {
            let names = cap.get(1).unwrap().as_str();
            return names.split(",").map(|word| word.trim().to_owned());
        })
        .collect();
}

fn resolve_core<'s>(
    name: &str,
    modules: &'s BTreeMap<String, Module>,
    done: &mut HashSet<String>,
    result: &mut Vec<&'s Module>,
) {
    if result.iter().any(|m| m.name == name) {
        return;
    }

    let module = modules
        .get(name)
        .expect(&format!("Unknown module '{}'.", name));

    for name in &module.parents {
        resolve_core(name, modules, done, result);
    }

    result.push(module);
}

fn resolve<'s>(modules: &'s BTreeMap<String, Module>, names: &[&str]) -> Vec<&'s Module> {
    let mut done = HashSet::new();
    let mut result = Vec::new();
    for ref name in names {
        resolve_core(name, modules, &mut done, &mut result);
    }
    result
}

fn go() -> Result<(), MyError> {
    let library_dir_name = "lib";
    let package_file_path = "pcpack-packages.toml";
    let target_file_name = "program.txt";
    let output_file_name = "program-output.txt";

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
            let path = module["path"].as_str().unwrap();
            eprintln!("Module '{}' in '{}'.", name, path);

            let parents = match module.get("use") {
                Some(&toml::Value::Array(ref names)) => names
                    .into_iter()
                    .filter_map(|x| x.as_str())
                    .map(|x| x.to_owned())
                    .collect(),
                _ => Vec::new(),
            };

            let mut path_buf = PathBuf::new();
            path_buf.push(work_dir.as_path());
            path_buf.push(library_dir_name);
            path_buf.push(path);
            name_to_module.insert(
                name.to_owned(),
                Module {
                    name,
                    path: path_buf.to_owned(),
                    parents,
                },
            );
            let full_path = path_buf.to_str().unwrap();
            let status = if path_buf.as_path().exists() {
                format!("found at {}", full_path.to_owned())
            } else {
                format!("NOT FOUND at {}", full_path)
            };
            eprintln!(" --> {}", status);
        }
    }

    // Read the client file.
    let source = try!(read_all_text(Path::new(target_file_name)));
    eprintln!("\n> cat {}\n{}", target_file_name, source);

    // Find slot to embed code.
    let (source_before_slot, source_after_slot) =
        split_with_slot(&source).expect("Missing slot:begin/slot:end pair.");

    // Find `use` meta-statements.
    let mut slot = String::new();

    let required_module_names = find_required_module_names(&source);
    let use_modules = resolve(
        &name_to_module,
        required_module_names
            .iter()
            .map(|name| name.as_str())
            .collect::<Vec<_>>()
            .as_slice(),
    );

    for module in use_modules {
        let content = try!(read_all_text(module.path.as_path()));
        slot.write_str("\n").unwrap();
        slot.write_str(&content).unwrap();
        slot.write_str("\n").unwrap();
    }

    // Output.
    let mut output = String::new();
    output += &source_before_slot;
    output += &slot;
    output += &source_after_slot;
    try!(write_all_text(Path::new(output_file_name), &output));

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

#[test]
fn test_split_with_slot() {
    let source = r#"
import something;

// pcpack slot:begin

some code

# pcpack slot:end

print("Hello, world!");
"#;

    let (first, second) = split_with_slot(source).expect("It should find slot tags.");
    assert_eq!(
        first,
        r#"
import something;

// pcpack slot:begin"#
    );
    assert_eq!(
        second,
        r#"# pcpack slot:end

print("Hello, world!");
"#
    );
}

#[test]
fn test_find_required_module_names() {
    let source = r#"
import something;

// pcpack use ch

# pcpack use hello-world, anti-gravity

print("Hello, world!");
"#;

    let texts = find_required_module_names(source);
    assert_eq!(texts, vec!["ch", "hello-world", "anti-gravity"]);
}

#[test]
fn test_resolver() {
    let modules = {
        let mut modules = BTreeMap::new();
        {
        let mut add = |name: &str, parents: Vec<&str>| {
            modules.insert(
                name.to_owned(),
                Module {
                    name: name.to_owned(),
                    path: PathBuf::from(name.to_owned() + ".txt"),
                    parents: parents.into_iter().map(|name| name.to_owned()).collect(),
                },
            );
        };
        add("a1", vec![]);
        add("a2", vec![]);
        add("a3", vec![]);
        add("b1", vec!["a1", "a2"]);
        add("c1", vec!["b1", "a2"]);
        add("d1", vec!["b1", "c1"]);
        }
        modules
    };

    let list = resolve(&modules, &vec!["d1"]);

    assert_eq!(
        list.into_iter().map(|m| m.name.as_str()).collect::<Vec<&str>>(),
        vec!["a1", "a2", "b1", "c1", "d1"]
    );
}
