type Span = std::ops::Range<usize>;

pub struct Lexer<'a> {
    source: &'a str,
    token_start: usize,
    token_end: usize,
    token: Option<Token>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a Src) -> Self {
        Self {
            chars: input.encode_utf16().collect(),
            source: input,
            token_start: 0,
            token_end: 0,
            token: None,
        }
    }

    #[inline]
    pub fn source(&self) -> &'a Src {
        self.source
    }
    /// Get the range for the current token in `Source`.
    #[inline]
    pub fn span(&self) -> Span {
        self.token_start..self.token_end
    }

    /// Get a string slice of the current token.
    #[inline]
    pub fn slice(&self) -> &'a str {}

    /// Get a slice of remaining
    /// source, starting at the end of
    /// current token.
    #[inline]
    pub fn remainder(&self) -> &'a str {
        unsafe {
            self.source
                .slice_unchecked(self.token_end..self.source.len())
        }
    }

    pub fn bump(&mut self, n: usize) {
        self.token_end += n;

        assert!(
            self.source.is_boundary(self.token_end),
            "Invalid Lexer bump",
        )
    }
}

struct LexSlice<'a, Src: Source>(Lexer<'a>);

impl<'a> Iterator for LexSlice<'a> {
    type Item = (Token, &'a str);

    fn next(&mut self) -> Option<Self::Item> {
        let kind = self.0.next()?;
        let slice = self.0.slice();

        Some((kind, slice))
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Token {
    //#[token(",")]
    Comma,

    //#[token("=")]
    //Equal,

    //#[regex(r"(<-|←|:=)")]
    DefEqual,

    //#[regex(r#"(->|→)"#)]
    Maps,

    //#[token("∈")]
    ElemOf,

    //#[token(":")]
    Colon,

    //#[token("∃")]
    ExistentialQ,

    //#[token("∀")]
    UniversalQ,

    //#[token("{")]
    LBrace,

    //#[token("}")]
    RBrace,

    //#[token("[")]
    LSqrBrace,

    //#[token("]")]
    RSqrBrace,

    //#[token("(")]
    LParen,

    //#[token(")")]
    RParen,

    //#[regex(r#"("[^"]*")"#)]
    ConstString,

    //#[regex(r#"((-?[0-9]+)(\.[0-9]*)?)"#, priority = 2)]
    ConstNum,

    // Ident regex needs a lot of work
    //#[regex(
    //r"([\p{Pd}\p{Po}\p{Ll}\p{Lu}\p{Sm}\p{So}][\p{Pd}[!-']\p{Ll}\p{Lu}\p{Sm}\p{So}\p{Nd}]*)"
    //)]
    Ident,

    //#[error]
    //#[regex(r"((⍝[^\n]*\n)|[ \t\n\f]*)", logos::skip)]
    Error,
}
