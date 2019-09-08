use super::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct DotRule {
    rule_id: RuleId,
    dot: usize,

    /// 先読み
    /// この生成規則を解析し終わった後にあるトークン
    token: Token,
}

impl DotRule {
    pub(crate) fn new(rule_id: RuleId, token: Token) -> Self {
        DotRule {
            rule_id,
            dot: 0,
            token,
        }
    }

    fn rule<'a>(&self, grammar: &'a Grammar) -> &'a Rule {
        &grammar.rules()[self.rule_id]
    }

    pub(crate) fn source(&self, grammar: &Grammar) -> NonTerm {
        self.rule(grammar).source()
    }

    pub(crate) fn len(&self, grammar: &Grammar) -> usize {
        self.rule(grammar).target().len()
    }

    pub(crate) fn token(&self) -> &Token {
        &self.token
    }

    fn dot_max(&self, grammar: &Grammar) -> usize {
        self.rule(grammar).target().len()
    }

    pub(crate) fn is_completed(&self, grammar: &Grammar) -> bool {
        self.dot >= self.dot_max(grammar)
    }

    /// ドットの右にある記号
    pub(crate) fn next_symbol<'a>(&self, grammar: &'a Grammar) -> Option<&'a Symbol> {
        self.rule(grammar).target().get(self.dot)
    }

    /// ドットの右にある記号より後にある記号の列
    pub(crate) fn after_next<'a>(&self, grammar: &'a Grammar) -> &'a [Symbol] {
        let i = min(self.dot + 1, self.dot_max(grammar));
        &self.rule(grammar).target()[i..]
    }

    /// ドットを1つ右に移動する。
    pub(crate) fn advance(mut self, grammar: &Grammar) -> Self {
        if !self.is_completed(grammar) {
            self.dot += 1;
        }
        self
    }
}
