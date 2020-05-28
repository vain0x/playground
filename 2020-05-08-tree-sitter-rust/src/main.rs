use std::io::{self, Write};
use tree_sitter::{Language, Node, Parser, Point, Range, Tree};

extern "C" {
    fn tree_sitter_jacco() -> Language;
}

fn write_indent(indent: usize, out: &mut Vec<u8>) -> io::Result<()> {
    for _ in 0..indent {
        write!(out, "  ")?;
    }
    Ok(())
}

fn write_point(point: Point, out: &mut Vec<u8>) -> io::Result<()> {
    write!(out, "{}:{}", point.row + 1, point.column + 1)
}

fn write_range(range: Range, out: &mut Vec<u8>) -> io::Result<()> {
    write_point(range.start_point, out)?;
    write!(out, "..")?;
    write_point(range.end_point, out)
}

fn write_node(text: &str, node: &Node, out: &mut Vec<u8>) -> io::Result<()> {
    let mut cursor = node.walk();
    let mut indent = 0;

    'a: loop {
        if indent >= 1 {
            write!(out, "\n")?;
            write_indent(indent, out)?;
        }

        let node = cursor.node();

        if cursor.goto_first_child() {
            write!(out, "- {}:  # ", node.kind())?;
            write_range(node.range(), out)?;
            indent += 1;
            continue;
        }

        // not having first child, node is a token.
        let word = node.utf8_text(text.as_bytes()).unwrap_or("???");
        write!(out, "- {:?}  # ", word)?;
        write_range(node.range(), out)?;

        loop {
            if cursor.goto_next_sibling() {
                continue 'a;
            }

            if cursor.goto_parent() {
                assert!(indent >= 1);
                indent -= 1;
                continue;
            }

            break 'a;
        }
    }

    assert_eq!(indent, 0);
    Ok(())
}

fn dump_node(text: &str, node: &Node) -> String {
    let mut out = vec![];
    write_node(text, node, &mut out).unwrap();
    unsafe { String::from_utf8_unchecked(out) }
}

fn main() {
    let mut parser = Parser::new();

    let language = unsafe { tree_sitter_jacco() };
    parser.set_language(language).unwrap();

    let source_code = {
        use std::io::Read;
        let mut stdin = std::io::stdin();
        let mut s = String::new();
        stdin.read_to_string(&mut s).unwrap();
        s
    };
    let tree: Tree = parser.parse(&source_code, None).unwrap();
    let root_node: Node = tree.root_node();

    assert_eq!(root_node.kind(), "source_file");
    assert_eq!(root_node.start_position().column, 0);

    let s = dump_node(&source_code, &root_node);
    println!("# See grammar.json for the grammar definition.");
    println!("# See input.txt for the source code.");
    println!("{}", s);
}
