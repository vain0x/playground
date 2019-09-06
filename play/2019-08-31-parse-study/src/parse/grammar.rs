//! 生成規則

use std::fmt::{self, Debug, Formatter};

/// 終端記号
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct NonTerm(&'static str);

/// シンボル
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum Symbol {
    Token(Token),
    NonTerm(NonTerm),
}

/// 生成規則
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Rule {
    source: NonTerm,
    target: Vec<Symbol>,
}

/// 文法
#[derive(Clone, Debug)]
pub(crate) struct Grammar {
    rules: Vec<Rule>,
}

impl Token {
    pub(crate) fn all() -> &'static [Token] {
        &[
            Token::Eof,
            Token::Int,
            Token::Ident,
            Token::ParenL,
            Token::ParenR,
            Token::Eq,
            Token::Plus,
            Token::Semi,
            Token::Star,
            Token::Print,
            ]
    }

    pub(crate) fn as_str(self) -> &'static str {
        match self {
            Token::Eof => "EOF",
            Token::Int => "INT",
            Token::Ident => "ID",
            Token::ParenL => "'('",
            Token::ParenR => "')'",
            Token::Eq => "'='",
            Token::Plus => "'+'",
            Token::Semi => "';'",
            Token::Star => "'*'",
            Token::Print => "PRINT",
        }
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl NonTerm {
    pub(crate) fn new(name: &'static str) -> Self {
        NonTerm(name)
    }

    pub(crate) fn to_rule(self, target: Vec<Symbol>) -> Rule {
        Rule::new(self, target)
    }
}

impl Debug for NonTerm {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str(self.0)
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

impl Debug for Symbol {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Symbol::Token(token) => f.write_fmt(format_args!("{:?}", token)),
            Symbol::NonTerm(non_term) => f.write_fmt(format_args!("{:?}", non_term)),
        }
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

impl Debug for Rule {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_fmt(format_args!("{:?} →", self.source()))?;

        for symbol in self.target() {
            f.write_fmt(format_args!(" {:?}", symbol))?;
        }

        Ok(())
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
