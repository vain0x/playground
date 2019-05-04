use super::*;

/// パースの対象となるトークンのリスト
pub(crate) struct TokenSource {
    source: String,
    /// i 番目のトークンの位置 (バイト単位)
    text_offsets: Vec<usize>,
    /// 空白やコメントではないトークンのリスト
    tokens: Vec<Token>,
}

impl TokenSource {
    pub(crate) fn new(source: String, raw_tokens: &[Token]) -> Self {
        let mut tokens = vec![];
        let mut text_offsets = vec![];
        let mut text_offset = 0;

        for &token in raw_tokens {
            if !token.kind.is_trivia() {
                tokens.push(token);
                text_offsets.push(text_offset);
            }
            text_offset += token.len;
        }

        TokenSource {
            source,
            text_offsets,
            tokens,
        }
    }

    /// i 番目のトークンの種類 (存在しなければ EOF)
    pub(crate) fn token_kind(&self, i: usize) -> SyntaxKind {
        if i >= self.tokens.len() {
            return SyntaxKind::EofToken;
        }
        self.tokens[i].kind
    }
}
