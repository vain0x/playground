use super::*;

pub(crate) struct TreeBuilder {
    stack: Vec<SyntaxKind>,
}

impl TreeBuilder {
    pub(crate) fn start_node(&mut self, kind: SyntaxKind) {
        self.stack.push(kind);
        eprintln!("START({:?})", kind);
    }

    pub(crate) fn finish_node(&mut self) {
        let kind = self.stack.pop();
        eprintln!("END({:?})", kind);
    }

    pub(crate) fn token(&mut self, kind: SyntaxKind, text: &str) {
        eprintln!("TOKEN({kind:?}) '{text}'", kind = kind, text = text);
    }

    fn error(&mut self, error: ParseError) {
        eprintln!("error {:?}", error);
    }
}

enum State {
    /// 初期状態
    PendingStart,
    Normal,
    PendingFinish,
}

/// パーサーからイベントを受け取るもの
pub(crate) struct TreeSink {
    text: String,
    tokens: Vec<Token>,
    state: State,
    text_pos: usize,
    token_pos: usize,
    inner: TreeBuilder,
}

impl TreeSink {
    pub(crate) fn new(source: String, tokens: Vec<Token>) -> Self {
        TreeSink {
            text: source,
            tokens,
            state: State::PendingStart,
            text_pos: 0,
            token_pos: 0,
            inner: TreeBuilder { stack: vec![] },
        }
    }

    /// start イベントを受け取ったとき
    pub(crate) fn start_node(&mut self, kind: SyntaxKind) {
        match std::mem::replace(&mut self.state, State::Normal) {
            State::PendingStart => {
                // root ノードの開始
                self.inner.start_node(kind);
                return;
            }
            State::PendingFinish => self.inner.finish_node(),
            State::Normal => {}
        }

        // トリビア (空白やコメント) の個数とバイト数と数える
        let n_trivias = self.tokens[self.token_pos..]
            .iter()
            .take_while(|it| it.kind.is_trivia())
            .count();
        let leading_trivias = &self.tokens[self.token_pos..self.token_pos + n_trivias];
        let mut trivia_end = self.text_pos + leading_trivias.iter().map(|it| it.len).sum::<usize>();
        // トリビアをまとめてスキップ
        self.eat_n_trivias(n_trivias);

        self.inner.start_node(kind);
    }

    /// finish イベントを受け取ったとき
    pub(crate) fn finish_node(&mut self) {
        match std::mem::replace(&mut self.state, State::PendingFinish) {
            State::PendingStart => unreachable!(),
            State::PendingFinish => self.inner.finish_node(),
            State::Normal => (),
        }
    }

    /// 可能なかぎりトリビアを飛ばす
    fn eat_trivias(&mut self) {
        while let Some(&token) = self.tokens.get(self.token_pos) {
            if !token.kind.is_trivia() {
                break;
            }
            self.do_token(token.kind, token.len, 1);
        }
    }

    fn eat_n_trivias(&mut self, n: usize) {
        for _ in 0..n {
            let token = self.tokens[self.token_pos];
            assert!(token.kind.is_trivia());
            self.do_token(token.kind, token.len, 1);
        }
    }

    /// token イベントを受け取ったとき
    pub(crate) fn token(&mut self, kind: SyntaxKind, n_tokens: u8) {
        match std::mem::replace(&mut self.state, State::Normal) {
            State::PendingStart => unreachable!(),
            State::PendingFinish => self.inner.finish_node(),
            State::Normal => (),
        }
        self.eat_trivias();
        let n_tokens = n_tokens as usize;
        let len = self.tokens[self.token_pos..self.token_pos + n_tokens]
            .iter()
            .map(|it| it.len)
            .sum::<usize>();
        self.do_token(kind, len, n_tokens);
    }

    /// トークンを読んだとき
    fn do_token(&mut self, kind: SyntaxKind, text_len: usize, token_len: usize) {
        let range = self.text_pos..self.text_pos + text_len;
        self.inner.token(kind, &self.text[range]);

        self.text_pos += text_len;
        self.token_pos += token_len;
    }

    pub(crate) fn error(&mut self, error: ParseError) {
        self.inner.error(error)
    }

    pub(crate) fn finish(mut self) {
        match std::mem::replace(&mut self.state, State::Normal) {
            State::PendingFinish => {
                self.eat_trivias();

                // root ノードの完了
                self.inner.finish_node();
            }
            State::PendingStart | State::Normal => unreachable!(),
        }
    }
}
