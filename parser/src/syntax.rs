use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};

#[derive(Debug, Copy, Clone, PartialEq, FromPrimitive, ToPrimitive)]
pub enum SyntaxKind {
    Root,
    Error,
    Comment,

    LitNum,
    LitString,
    Ident,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LSqrBrace,
    RSqrBrace,
    Equal,
    DefEqual,
    ElemOf,
    Colon,
    Comma,
    Maps,

    Statement,
    Expression,
    Tuple,
    Function,
    VariableRef,
    VariableDef,
    Set,
    Predicate,
    FunctionDef,
    Mapping,
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}

pub type SyntaxNode = rowan::SyntaxNode<SamslangLanguage>;

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum SamslangLanguage {}

impl rowan::Language for SamslangLanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        Self::Kind::from_u16(raw.0).unwrap()
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind.to_u16().unwrap())
    }
}
