//! 生成規則

/// 終端記号
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum Token {
    /// 入力の終わり ($)
    Eof,
    /// 整数
    Int,
    /// 識別子
    Ident,
    /// (
    ParenL,
    /// )
    ParenR,
    /// =
    Eq,
    /// +
    Plus,
    /// ;
    Semi,
    /// *
    Star,
    /// print
    Print,
}

/// 非終端記号
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct NonTerm(&'static str);

/// シンボル
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum Symbol {
    Token(Token),
    NonTerm(NonTerm),
}

/// 生成規則
#[derive(Clone, Debug)]
pub(crate) struct Rule {
    source: NonTerm,
    target: Vec<Symbol>,
}

/// 文法
#[derive(Clone, Debug)]
pub(crate) struct Grammar {
    rules: Vec<Rule>,
}

impl NonTerm {
    pub(crate) fn new(name: &'static str) -> Self {
        NonTerm(name)
    }

    pub(crate) fn to_rule(self, target: Vec<Symbol>) -> Rule {
        Rule::new(self, target)
    }
}

impl From<Token> for Symbol {
    fn from(token: Token) -> Self {
        Symbol::Token(token)
    }
}

impl From<NonTerm> for Symbol {
    fn from(non_term: NonTerm) -> Self {
        Symbol::NonTerm(non_term)
    }
}

impl Rule {
    pub(crate) fn new(source: NonTerm, target: Vec<Symbol>) -> Self {
        Rule { source, target }
    }

    pub(crate) fn source(&self) -> NonTerm {
        self.source
    }

    pub(crate) fn target(&self) -> &[Symbol] {
        &self.target
    }
}

impl Grammar {
    pub(crate) fn new() -> Self {
        Grammar { rules: vec![] }
    }

    pub(crate) fn rules(&self) -> &[Rule] {
        &self.rules
    }

    pub(crate) fn add_rule(&mut self, rule: Rule) {
        self.rules.push(rule);
    }
}
