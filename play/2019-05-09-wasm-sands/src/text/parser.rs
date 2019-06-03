use super::*;
use std::cell::Cell;
use std::cmp::min;
use std::mem::replace;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct EventId(usize);

pub(crate) enum Event {
    Error(String),
    Start { kind: SynKind },
    Token,
    Finish,
}

pub(crate) enum EventData {
    Error(String),
    Start {
        kind: Option<SynKind>,
        preceder: Option<EventId>,
    },
    Token,
    Finish,
}

pub(crate) struct OpenNode {
    event_id: EventId,
}

pub(crate) struct ClosedNode {
    event_id: EventId,
}

pub(crate) struct Parser<'a> {
    text: &'a str,
    tokens: &'a [Token],
    spans: &'a [(usize, usize)],
    index: usize,
    strs: Vec<Option<String>>,
    events: Vec<EventData>,
    tick: Cell<usize>,
}

impl<'a> Parser<'a> {
    fn new(text: &'a str, tokens: &'a [Token], spans: &'a [(usize, usize)]) -> Self {
        Parser {
            text,
            tokens,
            spans,
            index: 0,
            strs: vec![],
            events: Vec::with_capacity(tokens.len()),
            tick: Cell::new(0),
        }
    }

    fn detect_infinite_loop(&self) {
        let tick = self.tick.get();
        self.tick.set(tick + 1);

        if tick >= 10_000_000 {
            panic!("Infinite loop?");
        }
    }

    pub(crate) fn nth_kind(&self, offset: usize) -> TokenKind {
        self.detect_infinite_loop();

        let i = self.index + offset;
        if i >= self.tokens.len() {
            return TokenKind::Eof;
        }

        self.tokens[i].kind
    }

    pub(crate) fn next(&self) -> TokenKind {
        self.nth_kind(0)
    }

    pub(crate) fn next2(&self) -> (TokenKind, TokenKind) {
        (self.nth_kind(0), self.nth_kind(1))
    }

    pub(crate) fn next3(&self) -> (TokenKind, TokenKind, TokenKind) {
        (self.nth_kind(0), self.nth_kind(1), self.nth_kind(2))
    }

    pub(crate) fn nth_text(&self, offset: usize) -> &str {
        let i = self.index + offset;
        if i >= self.tokens.len() {
            return "";
        }

        let (l, r) = self.spans[i];
        &self.text[l..r]
    }

    pub(crate) fn advance(&mut self, offset: usize) {
        let n = min(offset, self.tokens.len() - self.index);

        self.index += n;
        for _ in 0..n {
            self.events.push(EventData::Token);
        }
    }

    pub(crate) fn bump(&mut self) {
        self.advance(1);
    }

    pub(crate) fn start_node(&mut self) -> OpenNode {
        let event_id = EventId(self.events.len());
        self.events.push(EventData::Start {
            preceder: None,
            kind: None,
        });
        OpenNode { event_id }
    }

    fn finish_node(&mut self, event_id: EventId, kind: SynKind) -> ClosedNode {
        self.set_kind(event_id, kind);
        self.events.push(EventData::Finish);
        ClosedNode { event_id }
    }

    fn start_preceding_node(&mut self, event_id: EventId) -> OpenNode {
        let preceder = self.start_node();
        self.set_preceder(event_id, preceder.event_id);
        preceder
    }

    fn set_kind(&mut self, event_id: EventId, kind: SynKind) {
        match &mut self.events[event_id.0] {
            EventData::Start {
                kind: ref mut slot, ..
            } => {
                assert!(slot.is_none());
                *slot = Some(kind);
            }
            _ => unreachable!(),
        }
    }

    fn set_preceder(&mut self, event_id: EventId, preceder: EventId) {
        match &mut self.events[event_id.0] {
            EventData::Start {
                preceder: ref mut slot,
                ..
            } => {
                assert!(slot.is_none());
                *slot = Some(preceder);
            }
            _ => unreachable!(),
        }
    }

    pub(crate) fn raise_events(mut self, h: &mut impl FnMut(Event)) {
        fn collect_preceders(i: usize, events: &mut [EventData], preceders: &mut Vec<SynKind>) {
            match events[i] {
                EventData::Start {
                    kind: ref mut slot,
                    preceder,
                } => {
                    let kind = slot.take().unwrap();
                    preceders.push(kind);

                    if let Some(EventId(p)) = preceder {
                        collect_preceders(p, events, preceders);
                    }
                }
                _ => {}
            }
        }

        let mut ancestors = vec![];

        for i in 0..self.events.len() {
            match self.events[i] {
                EventData::Error(ref mut msg) => h(Event::Error(replace(msg, String::new()))),
                EventData::Start { kind: None, .. } => {}
                EventData::Start { .. } => {
                    ancestors.clear();
                    collect_preceders(i, &mut self.events, &mut ancestors);

                    while let Some(kind) = ancestors.pop() {
                        h(Event::Start { kind });
                    }
                }
                EventData::Token => h(Event::Token),
                EventData::Finish => h(Event::Finish),
            }
        }
    }

    pub(crate) fn error(&mut self, msg: &str) {
        self.events.push(EventData::Error(msg.to_string()));
    }
}

impl OpenNode {
    pub(crate) fn complete(self, kind: SynKind, p: &mut Parser<'_>) -> ClosedNode {
        p.finish_node(self.event_id, kind)
    }

    pub(crate) fn abandon(self, p: &mut Parser<'_>) {
        // OK: We don't need remove event because it does nothing unless complete.
    }
}

impl ClosedNode {
    pub(crate) fn precede(self, p: &mut Parser<'_>) -> OpenNode {
        p.start_preceding_node(self.event_id)
    }
}

pub(crate) fn do_parse(
    text: &str,
    tokens: &[Token],
    spans: &[(usize, usize)],
    g: impl FnOnce(&mut Parser<'_>),
    h: &mut impl FnMut(Event),
) {
    let mut parser = Parser::new(text, tokens, spans);
    g(&mut parser);
    parser.raise_events(h);
}
