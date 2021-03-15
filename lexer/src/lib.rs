mod custom;

pub use logos;
use logos::Logos;

pub struct Lexer<'a> {
    inner: logos::Lexer<'a, Token>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            inner: Token::lexer(input),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = (Token, &'a str);

    fn next(&mut self) -> Option<Self::Item> {
        let kind = self.inner.next()?;
        let text = self.inner.slice();

        Some((kind, text))
    }
}

#[derive(Logos, Debug, PartialEq)]
pub enum Token {
    #[token(",")]
    Comma,

    //#[token("=")]
    //Equal,
    #[regex(r"(<-|←|:=)")]
    DefEqual,

    #[regex(r#"(->|→)"#)]
    Maps,

    #[token("∈")]
    ElemOf,

    #[token(":")]
    Colon,

    #[token("∃")]
    ExistentialQ,

    #[token("∀")]
    UniversalQ,

    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token("[")]
    LSqrBrace,

    #[token("]")]
    RSqrBrace,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[regex(r#"("[^"]*")"#)]
    ConstString,

    #[regex(r#"((-?[0-9]+)(\.[0-9]*)?)"#, priority = 2)]
    ConstNum,

    // Ident regex needs a lot of work
    #[regex(
        r"([\p{Pd}\p{Po}\p{Ll}\p{Lu}\p{Sm}\p{So}][\p{Pd}[!-']\p{Ll}\p{Lu}\p{Sm}\p{So}\p{Nd}]*)"
    )]
    Ident,

    #[error]
    #[regex(r"((⍝[^\n]*\n)|[ \t\n\f]*)", logos::skip)]
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
        let mut lex = Token::lexer(
            r#"
            ⍝ Blah blah blah

            "#,
        );
        lexTest!(lex: None);
    }

    #[test]
    fn simple_set() {
        let mut lex = Token::lexer(
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
        let mut lex = Token::lexer(
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
        let mut lex = Token::lexer("<- ← :=");

        lexTest!(lex with Token:
            DefEqual,
            DefEqual,
            DefEqual
        );
    }
}
