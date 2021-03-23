#![warn(
    clippy::all,
    clippy::restriction,
    clippy::pedantic,
    clippy::nursery,
    clippy::cargo
)]

use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Tuple(Box<(Type, Type)>);
/// return a tuple of 2 types
pub fn tuple(a: Type, b: Type) -> Type {
    Type::Tuple(Tuple(Box::new((a, b))))
}

impl Tuple {
    pub fn cardinality(&self) -> Cardinal {
        (*self.0).0.cardinality() * (*self.0).1.cardinality()
    }
}

#[derive(Debug, Clone)]
pub enum Predicate {
    TypeEq(Type),
}

impl Predicate {
    pub fn cardinality(&self) -> Cardinal {
        match self {
            Predicate::TypeEq(t) => t.cardinality(),
        }
    }

    pub fn eval(&self) -> bool {
        match self {
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Set(Vec<Predicate>);

impl Set {
    pub fn cardinality(&self) -> Cardinal {
        self.0
            .iter()
            .fold(Cardinal::Finite(0), |i, p| i + p.cardinality())
    }
}

#[derive(Debug, Clone)]
pub struct Mapping(Box<(Type, Type)>);

#[derive(Debug, Clone)]
pub enum Type {
    Unit,
    Void,
    Tuple(Tuple),
    Set(Set),
    Mapping(Mapping),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Cardinal {
    Infinite,
    Finite(usize),
}
impl std::ops::Mul for Cardinal {
    fn mul(self, r: Cardinal) -> Cardinal {
        match (self, r) {
            (Cardinal::Infinite, _) | (_, Cardinal::Infinite) => Cardinal::Infinite,
            (Cardinal::Finite(a), Cardinal::Finite(b)) => Cardinal::Finite(a * b),
        }
    }
    type Output = Cardinal;
}

impl std::ops::Add for Cardinal {
    fn add(self, r: Cardinal) -> Cardinal {
        match (self, r) {
            (Cardinal::Infinite, _) | (_, Cardinal::Infinite) => Cardinal::Infinite,
            (Cardinal::Finite(a), Cardinal::Finite(b)) => Cardinal::Finite(a + b),
        }
    }
    type Output = Cardinal;
}

impl Type {
    pub fn cardinality(&self) -> Cardinal {
        match self {
            Type::Void => Cardinal::Finite(0),
            Type::Unit => Cardinal::Finite(1),
            Type::Tuple(t) => t.cardinality(),
            Type::Set(s) => s.cardinality(),
            Type::Mapping(_) => Cardinal::Finite(0),
        }
    }
}

impl std::ops::Add<Type> for Type {
    type Output = Type;
    fn add(self, b: Type) -> <Self as std::ops::Add<Type>>::Output {
        sum(self, b)
    }
}

impl std::ops::Mul<Type> for Type {
    fn mul(self, b: Type) -> <Self as std::ops::Mul<Type>>::Output {
        product(self, b)
    }
    type Output = Type;
}

pub fn product(a: Type, b: Type) -> Type {
    tuple(a, b)
}

pub fn sum(a: Type, b: Type) -> Type {
    Type::Set(Set(vec![Predicate::TypeEq(a), Predicate::TypeEq(b)]))
}

impl PartialEq<Type> for Type {
    fn eq(&self, r: &Type) -> bool {
        self.cardinality() == r.cardinality()
    }
}
impl Eq for Type {}

#[derive(Hash, Eq, PartialEq)]
struct Ident(String);

enum ExpressionUntyped {
    Ident(Ident),
    Dyadic(Box<(ExpressionUntyped, ExpressionUntyped, ExpressionUntyped)>),
    Monadic(Box<(ExpressionUntyped, ExpressionUntyped)>),
    Array(Vec<ExpressionUntyped>),
}

fn check_function_type(
    t: &(ExpressionUntyped, ExpressionUntyped, ExpressionUntyped),
    scope: &Scope,
) -> Option<Type> {
    match type_check(&t.1, scope) {
        Some(e) => match e {
            Type::Mapping(m) => {
                let ft = *(m.0);
                if ft.0 == type_check(&t.0, scope)? && ft.1 == type_check(&t.1, scope)? {
                    Some(ft.1)
                } else {
                    None
                }
            }
            _ => None,
        },
        _ => None,
    }
}

fn type_check(e: &ExpressionUntyped, scope: &Scope) -> Option<Type> {
    match e {
        ExpressionUntyped::Ident(i) => Some(scope.0.get(i)?.1.clone()),
        ExpressionUntyped::Dyadic(d) => check_function_type(d, scope),
        _ => None,
    }
}

struct Expression(ExpressionUntyped, Type);

struct Scope(HashMap<Ident, Expression>);

#[cfg(test)]
mod tests {
    use super::tuple;
    use super::Type;
    use crate::Cardinal;
    #[test]
    fn tuple_assoc() {
        assert_eq!(
            tuple(Type::Void, tuple(Type::Void, Type::Void)),
            tuple(tuple(Type::Void, Type::Void), Type::Void),
        );
    }

    use super::{product, sum};
    #[test]
    fn type_identity() {
        let plus = Type::Unit + Type::Void;
        eprintln!("Cardinality of {:?} = {:?}", plus, plus.cardinality());
        assert_eq!(Type::Unit + Type::Void, Type::Unit);
        assert_eq!(Type::Unit * Type::Void, Type::Void);
    }

    #[test]
    fn cardinal_add() {
        assert_eq!(
            Type::Unit.cardinality() + Type::Unit.cardinality(),
            Cardinal::Finite(2)
        );

        assert_eq!(
            Type::Unit.cardinality() + Type::Void.cardinality(),
            Cardinal::Finite(1)
        );
    }
}
