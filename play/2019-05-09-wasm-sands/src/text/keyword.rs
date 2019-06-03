use super::*;

const KEYWORDS: &[(Keyword, &str)] = &[
    (Keyword::Module, "module"),
    (Keyword::Func, "func"),
    (Keyword::Result, "result"),
    (Keyword::I32, "i32"),
    (Keyword::Const, "const"),
    (Keyword::Add, "add"),
];

impl Keyword {
    pub(crate) fn as_str(self) -> &'static str {
        KEYWORDS
            .iter()
            .filter_map(|&(k, word)| if k == self { Some(word) } else { None })
            .next()
            .unwrap_or("unknown")
    }

    pub(crate) fn parse(text: &str) -> Keyword {
        KEYWORDS
            .iter()
            .filter_map(|&(keyword, word)| if word == text { Some(keyword) } else { None })
            .next()
            .unwrap_or(Keyword::Unknown)
    }

    pub(crate) fn is_val_ty(self) -> bool {
        match self {
            Keyword::I32 => true,
            _ => false,
        }
    }
}
