use super::arena::Arena;

#[cfg(debug_assertions)]
type MyArena<T> = super::vec_arena::DebugVecArena<T>;

#[cfg(not(debug_assertions))]
type MyArena<T> = super::vec_arena::ReleaseVecArena<T>;

#[derive(Debug)]
struct NodeData {
    name: &'static str,
    left_opt: Option<Node>,
    right_opt: Option<Node>,
}

impl NodeData {
    fn new_leaf(name: &'static str) -> Self {
        NodeData {
            name,
            left_opt: None,
            right_opt: None,
        }
    }
}

type NodeArena = MyArena<NodeData>;

type Node = <NodeArena as Arena<NodeData>>::Id;

#[test]
pub(crate) fn main_test() {
    let mut nodes = NodeArena::new();
    let alice = nodes.insert(NodeData::new_leaf("Alice"));
    let bob = nodes.insert(NodeData::new_leaf("Bob"));
    let catherine = nodes.insert(NodeData {
        name: "Catherine",
        left_opt: Some(alice),
        right_opt: Some(bob),
    });

    assert_eq!(alice.of(&nodes).name, "Alice");
    assert_eq!(
        catherine.of(&nodes).right_opt.unwrap().of(&nodes).name,
        "Bob"
    );

    // We can obtain the content from id without passing the reference to arena -- only for debugging purpose.
    assert_eq!(
        format!("{:?}", alice),
        r##"NodeData { name: "Alice", left_opt: None, right_opt: None }"##
    );
}
