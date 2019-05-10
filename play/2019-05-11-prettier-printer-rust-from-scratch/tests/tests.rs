use prettier_printer::*;

#[test]
fn test_nil() {
    let mut p = Printer::new();
    let nil = Doc::nil(&mut p);
    assert_eq!(p.print(nil, PrintConfig::new()), "");
}

#[test]
fn test_concat_tests() {
    let mut printer = Printer::new();
    let p = &mut printer;
    let concat = Doc::text("hello".to_string(), p)
        .concat(Doc::text(", ".to_string(), p), p)
        .concat(Doc::text("world!".to_string(), p), p);
    assert_eq!(p.print(concat, PrintConfig::new()), "hello, world!");
}

#[test]
fn test_tree_example() {
    struct Node {
        text: &'static str,
        children: Vec<Node>,
    }

    impl Node {
        fn new(text: &'static str, children: Vec<Node>) -> Node {
            Node { text, children }
        }

        fn new_text(text: &'static str) -> Node {
            Self::new(text, vec![])
        }
    }

    fn show_tree(node: &Node, p: &mut Printer) -> Doc {
        Doc::text(node.text.to_string(), p)
            .concat(show_bracket(&node.children, p).nest(node.text.len(), p), p)
    }

    fn show_bracket(nodes: &[Node], p: &mut Printer) -> Doc {
        if nodes.is_empty() {
            Doc::nil(p)
        } else {
            Doc::text("[".to_string(), p)
                .concat(show_trees(nodes, p).nest(1, p), p)
                .concat(Doc::text("]".to_string(), p), p)
        }
    }

    fn show_trees(nodes: &[Node], p: &mut Printer) -> Doc {
        if nodes.len() == 1 {
            show_tree(&nodes[0], p)
        } else {
            show_tree(&nodes[0], p)
                .concat(Doc::text(",".to_string(), p), p)
                .concat(Doc::line(p), p)
                .concat(show_trees(&nodes[1..], p), p)
        }
    }

    let tree = Node::new(
        "aaa",
        vec![
            Node::new("bbbbb", vec![Node::new_text("ccc"), Node::new_text("dd")]),
            Node::new_text("eee"),
            Node::new(
                "ffff",
                vec![
                    Node::new_text("gg"),
                    Node::new_text("hhh"),
                    Node::new_text("ii"),
                ],
            ),
        ],
    );

    fn print(node: &Node, indent_size: usize, width: usize) -> String {
        let mut p = Printer::new();
        let doc = show_tree(node, &mut p);
        p.print(
            doc,
            PrintConfig::new()
                .with_indent_size(indent_size)
                .with_width(width),
        )
    }

    assert_eq!(
        print(&tree, 4, 80),
        r#"aaa[bbbbb[ccc,
          dd],
    eee,
    ffff[gg,
         hhh,
         ii]]"#
    );
}
