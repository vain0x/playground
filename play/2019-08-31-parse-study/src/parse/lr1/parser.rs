use super::*;

pub(crate) struct Parser {
    tokens: Vec<Token>,
    table: ParseTable,
    ti: usize,
    stack: Vec<StateId>,

    // For debugging.
    symbols: Vec<Symbol>,
}

impl Parser {
    pub(crate) fn new(tokens: Vec<Token>, table: ParseTable) -> Self {
        let stack = vec![table.initial_state()];
        let symbols = vec![NonTerm::new("S'").into()];

        Parser {
            tokens,
            table,
            ti: 0,
            stack,
            symbols,
        }
    }

    fn current_state(&self) -> StateId {
        self.stack
            .last()
            .cloned()
            .expect("NEVER: Stack never gets empty")
    }

    fn current_token(&self) -> Token {
        match self.tokens.get(self.ti) {
            None => Token::Eof,
            Some(&token) => token,
        }
    }

    fn do_reduce(&mut self, non_term: NonTerm, count: usize) {
        assert!(self.stack.len() > count);

        let mut symbols = vec![];

        for _ in 0..count {
            self.stack.pop();
            let symbol = self.symbols.pop().unwrap();

            symbols.push(symbol);
        }

        symbols.reverse();
        eprintln!("    還元 {:?} → {:?}", symbols, non_term)
    }

    pub(crate) fn parse(mut self) -> bool {
        loop {
            let state = self.current_state();
            let token = self.current_token();
            let action = self.table.on_token(state, token);

            eprintln!(
                "状態({:?}), 字句({:?}), 操作({:?}), スタック({:?})",
                state, token, action, self.symbols
            );

            match action {
                Action::Error => {
                    eprintln!("構文エラー");
                    return false;
                }
                Action::Accept => {
                    return true;
                }
                Action::Shift(next_state) => {
                    self.stack.push(next_state);
                    self.symbols.push(token.into());
                    self.ti += 1;
                    continue;
                }
                Action::Reduce { non_term, count } => {
                    // いま生成規則 source → t1 t2 ... tN の解析が完了している状態。
                    // t1, t2, ... に対応する状態をスタックから除去する。
                    // 結果的に source の解析を開始する前の状態に (ti 以外) 巻き戻る。
                    self.do_reduce(non_term, count);

                    // source の解析が完了したので、ドットが source を飛び越えた先の状態に遷移する。
                    match self.table.on_non_term(self.current_state(), non_term) {
                        Action::Go(next_state) => {
                            self.stack.push(next_state);
                            self.symbols.push(non_term.into());
                        }
                        action => {
                            panic!("還元後の動作は Go でなければいけない ({:?})", action);
                        }
                    }
                    continue;
                }
                Action::Go(..) => {
                    panic!("Go は還元によってしか引き起こされない");
                }
            }
        }
    }
}
