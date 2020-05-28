use super::*;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub(crate) struct EventId(usize);

impl EventId {
    pub(crate) fn new(id: usize) -> Self {
        EventId(id)
    }
}

impl From<EventId> for usize {
    fn from(event_id: EventId) -> Self {
        event_id.0
    }
}

#[derive(Debug)]
pub(crate) enum Event {
    // 具象構文木のノードを開始する。
    // このノードから次の Finish までのトークンは1個のノードとして扱われる。
    // ただし king = Tombstone のときは、このイベントは無視する。(というか、このイベントが不要になったときに kind が Tombstone に変更される。)
    // これが作るノードがどのノードの子ノードになるかは、forward_parent の値による。
    // forward_parent = None なら、直前の Start が作るノードの子ノードになる。
    // forward_parent = Some(n) なら、n 個目のイベント (Start) が作るノードの子ノードになる。
    // 例えば x * y + z は ((x * y) + z) という階層構造なので、次のようなツリーになる。
    //
    //         root
    //          |
    //          +
    //        /   \
    //       *      z
    //     /   \
    //   x       y
    //
    // これを構築するには本来、以下のような順番でイベントを発行する必要があるが、
    //
    // start(root)
    // start(+)
    // start(*)
    // token(x)
    // token(*)
    // token(y)
    // finish // *
    // token(+)
    // token(z)
    // finish // +
    // finish // root
    //
    // これではパーサーは * より先に + を発見する必要がある。つまりかなりの先読みが必要であり、実装が難しい。代わりに、次のようなイベントを発行していいことにする。
    //
    // 0: start(root, forward_parent = 2) --+
    // 1: token(x)                          |
    // 2: start(*, forward_parent = 3)  <---+
    // 3: token(*)                |
    // 4: token(y)                |
    // 5: start(+)             <--+
    // 6: token(+)
    // 7: token(z)
    // 8: finish // +
    // 9: finish // *
    // 10: finish // root
    //
    // このイベント列を forward_parent を利用して前述のようなイベント列に変形して、ツリーを構築する。
    Start {
        kind: SyntaxKind,
        forward_parent: Option<usize>,
    },

    // 直前の Start を終了して、その間にあるトークンの列を1個のノードとしてまとめる
    Finish,

    Token {
        kind: SyntaxKind,
        // n_raw_tokens: u8,
    },

    Error {
        msg: ParseError,
    },
}

impl Event {
    pub(crate) fn tombstone() -> Self {
        Event::Start {
            kind: SyntaxKind::Tombstone,
            forward_parent: None,
        }
    }
}

/// Generate the syntax tree with the control of events.
pub(super) fn process(sink: &mut TreeSink, mut events: Vec<Event>) {
    let mut forward_parents = Vec::new();

    for i in 0..events.len() {
        match std::mem::replace(&mut events[i], Event::tombstone()) {
            Event::Start {
                kind: SyntaxKind::Tombstone,
                ..
            } => (),

            Event::Start {
                kind,
                forward_parent,
            } => {
                // For events[A, B, C], B is A's forward_parent, C is B's forward_parent,
                // in the normal control flow, the parent-child relation: `A -> B -> C`,
                // while with the magic forward_parent, it writes: `C <- B <- A`.

                // append `A` into parents.
                forward_parents.push(kind);
                let mut idx = i;
                let mut fp = forward_parent;
                while let Some(fwd) = fp {
                    idx += fwd;
                    // append `A`'s forward_parent `B`
                    fp = match std::mem::replace(&mut events[idx], Event::tombstone()) {
                        Event::Start {
                            kind,
                            forward_parent,
                        } => {
                            if kind != SyntaxKind::Tombstone {
                                forward_parents.push(kind);
                            }
                            forward_parent
                        }
                        _ => unreachable!(),
                    };
                    // append `B`'s forward_parent `C` in the next stage.
                }

                for kind in forward_parents.drain(..).rev() {
                    sink.start_node(kind);
                }
            }
            Event::Finish => sink.finish_node(),
            Event::Token { kind } => {
                sink.token(kind, 1);
            }
            Event::Error { msg } => sink.error(msg),
        }
    }
}
