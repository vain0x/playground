#![allow(unused_imports)]

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

struct Config {
    /// A relative path from the config file to the file to be bundled (e.g., ``./main.rs``).
    bundle_path: PathBuf,
    /// A relative path from the config file to the root directory of library files.
    lib_root: PathBuf,
    modules: Vec<Module>,
}

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

fn read_config(config_path: &Path) -> Result<Config, MyError> {
    let content = try!(read_all_text(config_path));
    let toml = content.parse::<toml::Value>().expect(&format!(
        "Coundn't parse config file '{}'.",
        config_path.to_str().unwrap_or("?")
    ));

    let bundle_path = PathBuf::from(
        toml.get("bundle_path")
            .and_then(|t| t.as_str())
            .unwrap_or("main.rs"),
    );
    let lib_root = PathBuf::from(
        toml.get("lib_root")
            .and_then(|t| t.as_str())
            .unwrap_or("lib"),
    );
    let modules = toml.get("modules")
        .and_then(|t| t.as_array())
        .unwrap_or(&vec![])
        .into_iter()
        .map(|t| {
            let m = t.as_table().unwrap();
            let name = m["name"].as_str().unwrap().to_owned();
            let path = PathBuf::from(
                m["path"]
                    .as_str()
                    .map(|s| s.to_owned())
                    .unwrap_or_else(|| format!("{}.rs", name)),
            );
            let parents = m.get("use")
                .and_then(|t| t.as_array())
                .unwrap_or(&vec![])
                .into_iter()
                .map(|t| t.as_str().unwrap().to_owned())
                .collect::<Vec<_>>();
            Module {
                name,
                path,
                parents,
            }
        })
        .collect::<Vec<_>>();

    let config = Config {
        bundle_path,
        lib_root,
        modules,
    };

    return Ok(config);
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
    let config_path = PathBuf::from("pcpack-config.toml");
    let output_file_name = "program-output.txt";

    // Move working directory into `workspace`.
    let mut work_dir = try!(env::current_dir());
    {
        work_dir.push("workspace");
        try!(env::set_current_dir(&work_dir.as_path()));
        eprintln!("Current dir = {}", work_dir.display());
    }

    // Read the config file.
    let config = try!(read_config(config_path.as_path()));

    let name_to_module = {
        let mut name_to_module: BTreeMap<String, Module> = BTreeMap::new();
        for module in &config.modules {
            let name = &module.name;
            let path = &module.path;
            eprintln!("Module '{}' in '{}'.", name, path.display());

            let mut path_buf = PathBuf::from(&work_dir).join(&config.lib_root).join(path);

            name_to_module.insert(
                name.to_owned(),
                Module {
                    name: name.to_owned(),
                    path: path_buf.to_owned(),
                    parents: module.parents.to_owned(),
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
        name_to_module
    };

    // Read the client file.
    let source = try!(read_all_text(config.bundle_path.as_path()));
    eprintln!("\n> cat {}\n{}", config.bundle_path.to_str().unwrap_or("?"), source);

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
}

#[cfg(test)]
mod tests {
    use super::*;

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
            list.into_iter()
                .map(|m| m.name.as_str())
                .collect::<Vec<&str>>(),
            vec!["a1", "a2", "b1", "c1", "d1"]
        );
    }
}
