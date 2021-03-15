mod syntax;

use lexer::{Lexer, Token};
use rowan::Checkpoint;
use rowan::{GreenNode, GreenNodeBuilder};
use std::iter::Peekable;
use syntax::{SyntaxKind, SyntaxNode};

#[derive(Debug, Copy, Clone)]
pub(crate) enum ErrType {
    DefErr,
    UnmatchedBracket,
    Unrecognized,
    Unexpected,
    FuncErr,
}

impl ErrType {
    pub(crate) fn message(self, message: &str) -> ParseErr {
        ParseErr(self, message.to_string())
    }
}
pub struct ParseErr(ErrType, String);

impl ParseErr {
    pub fn message(&self) -> &str {
        &self.1
    }
}

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    builder: GreenNodeBuilder<'static>,
    errors: Vec<ParseErr>,
}

enum ParseRes {
    Ok,
    Error(ErrType),
    EOF,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            lexer: Lexer::new(input).peekable(),
            builder: GreenNodeBuilder::new(),
            errors: Vec::new(),
        }
    }

    pub fn parse(mut self) -> Parse {
        self.builder.start_node(SyntaxKind::Root.into());

        //println!("starting parse!");

        loop {
            match self.expression(self.builder.checkpoint(), |_| {}) {
                ParseRes::Ok => {}
                ParseRes::Error(_) => {}
                ParseRes::EOF => {
                    break;
                }
            }
        }

        //println!("Finishing parse");

        self.builder.finish_node();

        Parse {
            green_node: self.builder.finish(),
        }
    }

    //fn statement(&mut self) -> ParseRes {
    //self.builder.start_node(SyntaxKind::Statement.into());

    //match self.expression() {
    //ParseRes::EOF => {
    //self.builder.finish_node();
    //return ParseRes::EOF;
    //}
    //_ => {}
    //}

    //self.builder.finish_node();
    //ParseRes::Ok
    //}
    fn rparen(&mut self) {
        self.expect(Token::RParen);
    }

    fn expression<F: FnMut(&mut Self)>(&mut self, expr_cp: Checkpoint, mut f: F) -> ParseRes {
        let mut prev_cp = None;
        loop {
            match self.lexer.peek() {
                Some((Token::Ident, _))
                | Some((Token::ConstNum, _))
                | Some((Token::ConstString, _))
                | Some((Token::ElemOf, _)) => {
                    prev_cp = Some(self.builder.checkpoint());
                    //println!("Ident -> ");
                    self.bump();
                }
                Some((Token::LParen, _)) => {
                    prev_cp = Some(self.builder.checkpoint());
                    //println!("LParen -> ");
                    self.bump();
                    self.expression(prev_cp.unwrap(), Self::rparen);
                }
                Some((Token::DefEqual, _)) => {
                    if let Some(cp) = prev_cp {
                        //println!("Def equal -> ");
                        self.def_equal(cp);
                    } else {
                        return self.error(ErrType::DefErr, "Expected an Ident before '←' token");
                    }
                }
                Some((Token::Colon, _)) => {
                    if let Some(cp) = prev_cp {
                        self.func_def(cp);
                    } else {
                        return self.error(
                            ErrType::FuncErr,
                            "Expected an Ident before ':' function definition",
                        );
                    }
                }
                Some((Token::LBrace, _)) => {
                    prev_cp = Some(self.builder.checkpoint());
                    self.set();
                }
                None => match prev_cp {
                    None => {
                        return ParseRes::EOF;
                    }

                    Some(_) => {
                        self.builder
                            .start_node_at(expr_cp, SyntaxKind::Expression.into());
                        self.builder.finish_node();
                        f(self);
                        return ParseRes::EOF;
                    }
                },
                Some((Token::Comma, _)) => {
                    self.builder
                        .start_node_at(expr_cp, SyntaxKind::Expression.into());
                    self.bump();
                    self.builder.finish_node();
                    break;
                }
                Some((_, slice)) => match prev_cp {
                    None => {
                        let token = slice.clone();
                        let msg = format!("unrecognised token: {}", token);
                        return self.error(ErrType::Unrecognized, &msg);
                    }
                    Some(_) => {
                        self.builder
                            .start_node_at(expr_cp, SyntaxKind::Expression.into());
                        f(self);
                        self.builder.finish_node();
                        break;
                    }
                },
            }
        }
        ParseRes::Ok
    }

    fn func_def(&mut self, cp: Checkpoint) {
        self.builder
            .start_node_at(cp, SyntaxKind::FunctionDef.into());
        self.bump(); //COLON
        self.mapping(self.builder.checkpoint(), |_| {});
        self.pred();
        self.builder.finish_node();
    }

    fn mapping<F: FnMut(&mut Self)>(&mut self, cp: Checkpoint, mut f: F) {
        loop {
            match self.lexer.peek() {
                Some((Token::Ident, _)) => {
                    self.bump();
                }
                Some((Token::LParen, _)) => {
                    let m_cp = self.builder.checkpoint();
                    self.bump();
                    self.mapping(m_cp, Self::rparen);
                }
                Some((Token::Maps, _)) => {
                    self.bump();
                    break;
                }
                _ => {
                    self.error(ErrType::Unexpected, "Expected Ident, -> or (.");
                    return;
                }
            }
        }
        loop {
            match self.lexer.peek() {
                Some((Token::Ident, _)) => {
                    self.bump();
                }
                Some((Token::LParen, _)) => {
                    let m_cp = self.builder.checkpoint();
                    self.bump();
                    self.mapping(m_cp, Self::rparen);
                }
                _ => {
                    break;
                }
            }
        }

        f(self);
        self.builder.start_node_at(cp, SyntaxKind::Mapping.into());
        self.builder.finish_node();
    }

    fn error(&mut self, e: ErrType, m: &str) -> ParseRes {
        self.builder.start_node(SyntaxKind::Error.into());
        self.errors.push(e.message(m));
        self.bump();
        self.builder.finish_node();
        ParseRes::Error(e)
    }

    fn set(&mut self) {
        self.builder.start_node(SyntaxKind::Set.into());
        self.bump(); // Opening "{"

        // SET PARSE
        loop {
            match self.lexer.peek() {
                Some((Token::Ident, _)) => {
                    self.bump();
                }
                Some((Token::LSqrBrace, _)) => {
                    self.pred();
                    self.expect(Token::RBrace);
                    break;
                }
                Some((Token::RBrace, _)) => {
                    self.bump();
                    break;
                }
                _ => {
                    self.error(ErrType::Unrecognized, "Unexpected token in set!");
                }
            }
        }

        self.builder.finish_node();
    }

    fn pred(&mut self) {
        self.builder.start_node(SyntaxKind::Predicate.into());

        self.bump(); // Opening "["

        loop {
            match self.lexer.peek() {
                Some((Token::RSqrBrace, _)) => {
                    self.bump();
                    break;
                }
                _ => match self.expression(self.builder.checkpoint(), |_| {}) {
                    ParseRes::Ok => {}
                    ParseRes::Error(_) => {}
                    ParseRes::EOF => {
                        self.error(ErrType::UnmatchedBracket, "expected ']', found EOF");
                    }
                },
            }
        }

        self.builder.finish_node();
    }

    fn def_equal(&mut self, cp: Checkpoint) {
        self.bump();
        self.builder
            .start_node_at(cp, SyntaxKind::VariableDef.into());
        self.expression(self.builder.checkpoint(), |_| {});
        self.builder.finish_node();
    }

    fn bump(&mut self) {
        /// macro for converting from Token kind into syntax kind in bump function
        macro_rules! build_tokens {
            ( $($tok:ident -> $syn:ident),*) => {
                match self.lexer.next(){
                    $(Some((Token::$tok, slice)) => {self.builder.token(SyntaxKind::$syn.into(), slice);})*
                    Some((tok, _)) => {panic!(format!("Unimplemented bump: {:?}", tok));}
                    None => {panic!("Expected Some(Token) on bump. Found nothing");}
                }
            }
        }
        build_tokens! {
            Ident -> Ident,
            LParen -> LParen,
            RParen -> RParen,
            LBrace -> LBrace,
            RBrace -> RBrace,
            LSqrBrace -> LSqrBrace,
            RSqrBrace -> RSqrBrace,
            ConstNum -> LitNum,
            ConstString ->LitString,
            DefEqual -> DefEqual,
            ElemOf -> ElemOf,
            Colon -> Colon,
            Comma -> Comma,
            Maps -> Maps,


            Error -> Error
        }
    }

    fn expect(&mut self, tok: Token) {
        if let Some((actual_type, _)) = self.lexer.peek() {
            if actual_type == &tok {
                self.bump();
            } else {
                self.error(ErrType::Unexpected, &format!("Expected {:?}", tok));
            }
        } else {
            self.error(
                ErrType::Unexpected,
                format!("Expected Some({:?})", tok).as_str(),
            );
        }
    }
}

pub struct Parse {
    green_node: GreenNode,
}

impl Parse {
    pub fn debug_tree(&self) -> String {
        let syntax_node = SyntaxNode::new_root(self.green_node.clone());
        let formatted = format!("{:#?}", syntax_node);

        formatted[..formatted.len() - 1].to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::Parser;
    use expect_test::{expect, Expect};

    fn check(input: &str, expected_tree: Expect) {
        let parse = Parser::new(input).parse();

        expected_tree.assert_eq(&parse.debug_tree());
    }

    #[test]
    fn parse_nothing() {
        check("", expect![[r#"Root@0..0"#]]);
    }

    #[test]
    fn parse_number() {
        check(
            "123",
            expect![[r#"
  Root@0..3
    Expression@0..3
      LitNum@0..3 "123""#]],
        );
    }

    #[test]
    fn parse_def() {
        check(
            "n <- 123",
            expect![[r#"
  Root@0..6
    Expression@0..6
      VariableDef@0..6
        Ident@0..1 "n"
        DefEqual@1..3 "<-"
        Expression@3..6
          LitNum@3..6 "123""#]],
        );
    }

    #[test]
    fn parse_def_to_tuple() {
        check(
            "(x y z) ← 10",
            expect![[r#"
  Root@0..10
    Expression@0..10
      VariableDef@0..10
        Expression@0..5
          LParen@0..1 "("
          Ident@1..2 "x"
          Ident@2..3 "y"
          Ident@3..4 "z"
          RParen@4..5 ")"
        DefEqual@5..8 "←"
        Expression@8..10
          LitNum@8..10 "10""#]],
        );
    }

    #[test]
    fn parse_nested() {
        println!("10 * (5 ∪ (⍳ 3))");
        check(
            "10 * (5 ∪ (⍳ 3))",
            expect![[r#"
  Root@0..15
    Expression@0..15
      LitNum@0..2 "10"
      Ident@2..3 "*"
      Expression@3..15
        LParen@3..4 "("
        LitNum@4..5 "5"
        Ident@5..8 "∪"
        Expression@8..14
          LParen@8..9 "("
          Ident@9..12 "⍳"
          LitNum@12..13 "3"
          RParen@13..14 ")"
        RParen@14..15 ")""#]],
        )
    }

    #[test]
    fn parse_set() {
        check(
            "n ← { X [ X ∈ B ] }",
            expect![[r#"
  Root@0..14
    Expression@0..14
      VariableDef@0..14
        Ident@0..1 "n"
        DefEqual@1..4 "←"
        Expression@4..14
          Set@4..14
            LBrace@4..5 "{"
            Ident@5..6 "X"
            Predicate@6..13
              LSqrBrace@6..7 "["
              Expression@7..12
                Ident@7..8 "X"
                ElemOf@8..11 "∈"
                Ident@11..12 "B"
              RSqrBrace@12..13 "]"
            RBrace@13..14 "}""#]],
        );
    }

    #[test]
    fn parse_function_def() {
        check(
            "suc: x -> y [x ∈ ℤ, y ← x + 1]",
            expect![[r#"
  Root@0..25
    Expression@0..25
      FunctionDef@0..25
        Ident@0..3 "suc"
        Colon@3..4 ":"
        Mapping@4..8
          Ident@4..5 "x"
          Maps@5..7 "->"
          Ident@7..8 "y"
        Predicate@8..25
          LSqrBrace@8..9 "["
          Expression@9..17
            Ident@9..10 "x"
            ElemOf@10..13 "∈"
            Ident@13..16 "ℤ"
            Comma@16..17 ","
          Expression@17..24
            VariableDef@17..24
              Ident@17..18 "y"
              DefEqual@18..21 "←"
              Expression@21..24
                Ident@21..22 "x"
                Ident@22..23 "+"
                LitNum@23..24 "1"
          RSqrBrace@24..25 "]""#]],
        );
    }
}
