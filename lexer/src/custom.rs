use unicode_categories::UnicodeCategories;
type Span = std::ops::Range<usize>;

mod source;
use source::Source;

pub struct Lexer<'a, Src: Source> {
    source: &'a Src,
    token_start: usize,
    token_end: usize,
    token: Option<Token>,
}

impl<'a, Src: Source> Lexer<'a, Src> {
    pub fn new(input: &'a Src) -> Self {
        Self {
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
    pub fn slice(&self) -> &'a Src::Slice {
        unsafe { self.source.slice_unchecked(self.span()) }
    }

    /// Get a slice of remaining
    /// source, starting at the end of
    /// current token.
    #[inline]
    pub fn remainder(&self) -> &'a Src::Slice {
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

    #[inline]
    fn error(&mut self) {
        self.token_end = self.source.find_boundary(self.token_end);
        self.token = Some(Token::Error);
    }

    #[inline]
    fn end(&mut self) {
        self.token = None;
    }

    #[inline]
    fn set(&mut self, token: Token) {
        self.token = Some(token);
    }

    /// Read a `Chunk` at current position of the `Lexer`. If end
    /// of the `Source` has been reached, this will return `0`.
    #[inline]
    fn read<Chunk>(&self) -> Option<Chunk>
    where
        Chunk: source::Chunk<'a>,
    {
        self.source.read(self.token_end)
    }

    /// Read a `Chunk` at a position offset by `n`.
    #[inline]
    fn read_at<Chunk>(&self, n: usize) -> Option<Chunk>
    where
        Chunk: source::Chunk<'a>,
    {
        self.source.read(self.token_end + n)
    }

    #[inline]
    unsafe fn read_unchecked<Chunk>(&self, n: usize) -> Chunk
    where
        Chunk: source::Chunk<'a>,
    {
        self.source.read_unchecked(self.token_end + n)
    }

    ///
    ///Test
    ///a
    ///chunk
    ///at
    ///current
    ///position
    ///with
    ///a
    ///closure.
    #[inline]
    fn test<T, F>(&self, test: F) -> bool
    where
        T: source::Chunk<'a>,
        F: FnOnce(T) -> bool,
    {
        match self.source.read::<T>(self.token_end) {
            Some(chunk) => test(chunk),
            None => false,
        }
    }

    ///
    //Test
    //a
    //chunk
    //at
    //current
    //position
    //offset
    //by
    //`n`
    //with
    //a
    //closure.
    #[inline]
    fn test_at<T, F>(&self, n: usize, test: F) -> bool
    where
        T: source::Chunk<'a>,
        F: FnOnce(T) -> bool,
    {
        match self.source.read::<T>(self.token_end + n) {
            Some(chunk) => test(chunk),
            None => false,
        }
    }

    fn lex(&mut self) {
        match self.read() {
            None => {
                self.end();
            }
        }
    }
}

impl<'a, Src: Source> Iterator for Lexer<'a, Src> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        //let kind = self.inner.next()?;
        //let text = self.inner.slice();
        self.token_start = self.token_end;

        self.lex();

        self.token
    }
}

struct LexSlice<'a, Src: Source>(Lexer<'a, Src>);

impl<'a, Src: Source> Iterator for LexSlice<'a, Src> {
    type Item = (Token, &'a Src::Slice);

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

#[cfg(test)]
mod tests {

    macro_rules! lexTest {
    (debug $lex:ident with $tok:ident $($( start @ $offset:literal )? : $($name:ident $(@ $spanl:literal .. $spanr:literal)? $(: $lit:literal)?),*)?) => {
        while let Some(tok) = $lex.next() {
            match tok{
                $tok::Error => {
                    println!("{}", format!(r#"Error@{:?}:"{}""#,  $lex.span(), $lex.slice()).red());
                }
                _ => {

                    println!(r#"{:?}@{:?}:"{}""#, tok, $lex.span(), $lex.slice());
                }
            }

        }
        assert!(false);
    };
    ($lex:ident : None) => {
        assert_eq!($lex.next(), None);
    };
    ($lex:ident with $tok:ident : $($name:ident $(@ $spanl:literal .. $spanr:literal)? $(: $lit:literal)?),*) => {
        $(
            {
                let tok = $lex.next();
                println!(r#"{:?}@{:?}:"{}""#, tok, $lex.span(), $lex.slice());
                assert_eq!(tok, Some($tok::$name));
                $(assert_eq!($lex.span(), $spanl .. $spanr+1);)?
                $(assert_eq!($lex.slice(), $lit);)?
            }
        )*
    };
    ($lex:ident with $tok:ident start@$offset:literal : $($name:ident $(@ $spanl:literal .. $spanr:literal)? $(: $lit:literal)?),*) => {
        $(
            {
                let tok = $lex.next();
                println!(r#"{:?}@{:?}:"{}""#, tok, $lex.span(), $lex.slice());
                assert_eq!(tok, Some($tok::$name));
                $(assert_eq!($lex.span(), ($spanl+$offset) .. ($spanr+$offset+1));)?
                $(assert_eq!($lex.slice(), $lit);)?
            }
        )*
    };
    }

    use super::{Lexer, Token};
    use logos::Logos;

    #[test]
    fn whitespace() {
        let mut lex = Lexer::new(
            r#"      
              
                
            "#,
        );
        lexTest!(lex: None);
    }

    #[test]
    fn comments() {
        let mut lex = Lexer::new(
            r#"
            ⍝ Blah blah blah

            "#,
        );
        lexTest!(lex: None);
    }

    #[test]
    fn simple_set() {
        let mut lex = Lexer::new(
            r#"Blah ← {X [X ∈ type1]}
            "#,
        );
        lexTest!(
          lex with Token start@0:
            Ident@0..3:"Blah",
            DefEqual@5..7:"←",
            LBrace@9..9:"{",
            Ident@10..10:"X",
            LSqrBrace@12..12:"[",
            Ident@13..13:"X",
            ElemOf@15..17:"∈",
            Ident@19..23:"type1",
            RSqrBrace@24..24:"]",
            RBrace@25..25:"}"
        );
    }

    #[test]
    fn const_expr() {
        let mut lex = Lexer::new(
            r#"
               a ← 0.45,
               b ← 5,
               c ← "const string",
               d ← 6,
               n ← (b d)
            "#,
        );

        lexTest!(lex with Token start@16:
            Ident@0..0:"a",
            DefEqual,
            ConstNum,
            Comma,

            Ident,
            DefEqual,
            ConstNum,
            Comma,

            Ident,
            DefEqual,
            ConstString,
            Comma,

            Ident,
            DefEqual,
            ConstNum,
            Comma,

            Ident,
            DefEqual,
            LParen,
            Ident,
            Ident,
            RParen
        );
    }

    #[test]
    fn lex_def_equality() {
        let mut lex = Lexer::new("<- ← :=");

        lexTest!(lex with Token:
            DefEqual,
            DefEqual,
            DefEqual
        );
    }
}
