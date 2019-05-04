use super::*;
use std::collections::*;

/// Start イベントが発行されていて、まだ Finish はしていないノードを表す。
// NOTE: complete メソッドを誤って2回呼ばないように、わざと Copy にしていない。
pub(crate) struct StartedNode {
    start_event_id: EventId,
}

impl StartedNode {
    pub(crate) fn complete(self, parser: &mut Parser, kind: SyntaxKind) -> FinishedNode {
        parser.complete_node(self.start_event_id, kind)
    }
}

/// Start/Finish イベントが発行済みのノードを表す。
// NOTE: precede メソッドを誤って2回呼ばないように、わざと Copy にしていない。
pub(crate) struct FinishedNode {
    kind: SyntaxKind,
    start_event_id: EventId,
    finish_event_id: EventId,
}

impl FinishedNode {
    pub(crate) fn precede(self, parser: &mut Parser) -> StartedNode {
        let node = parser.start_node();
        parser.set_node_forward_parent(&self, &node);
        node
    }
}

pub(crate) struct Parser {
    token_source: TokenSource,
    current: usize,
    events: Vec<Event>,
    event_completions: BTreeSet<EventId>,
    steps: std::cell::Cell<usize>,
}

impl Parser {
    fn detect_infinite_loop(&self) {
        self.steps.set(self.steps.get() + 1);
        assert!(self.steps.get() < 10_000_000, "the parser seems stuck");
    }

    fn look_ahead(&self, i: usize) -> SyntaxKind {
        self.detect_infinite_loop();

        self.token_source.token_kind(self.current + i)
    }

    pub(crate) fn next(&self) -> SyntaxKind {
        self.look_ahead(0)
    }

    /// いまのトークンを飛ばす。
    pub(crate) fn bump(&mut self) {
        self.do_bump(self.next())
    }

    fn do_bump(&mut self, kind: SyntaxKind) {
        self.current += 1;
        self.events.push(Event::Token { kind })
    }

    pub(crate) fn start_node(&mut self) -> StartedNode {
        let start_event_id = EventId::new(self.events.len());
        self.events.push(Event::tombstone());
        StartedNode { start_event_id }
    }

    fn complete_node(&mut self, start_event_id: EventId, kind: SyntaxKind) -> FinishedNode {
        self.event_completions.insert(start_event_id);
        self.set_node_kind(start_event_id, kind);

        let finish_event_id = EventId::new(self.events.len());
        self.events.push(Event::Finish);

        FinishedNode {
            kind,
            start_event_id,
            finish_event_id,
        }
    }

    /// Start イベントが開始するノードの SyntaxKind を設定する。
    fn set_node_kind(&mut self, event_id: EventId, kind: SyntaxKind) {
        let event = &mut self.events[usize::from(event_id)];
        match event {
            Event::Start {
                kind: ref mut slot, ..
            } => {
                *slot = kind;
            }
            _ => unreachable!(),
        }
    }

    /// Start イベントに forward_parent を設定する。
    fn set_node_forward_parent(&mut self, child: &FinishedNode, parent: &StartedNode) {
        let event = &mut self.events[usize::from(child.start_event_id)];
        let distance = usize::from(parent.start_event_id) - usize::from(child.start_event_id);
        match event {
            Event::Start {
                forward_parent: ref mut slot,
                ..
            } => {
                *slot = Some(distance);
            }
            _ => unreachable!(),
        }
    }
}

pub(crate) fn parse(token_source: TokenSource, grammar: impl Fn(&mut Parser)) -> Vec<Event> {
    let mut parser = Parser {
        token_source,
        current: 0,
        events: vec![],
        event_completions: BTreeSet::new(),
        steps: std::cell::Cell::new(0),
    };
    grammar(&mut parser);
    parser.events
}
