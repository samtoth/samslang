#![warn(
    clippy::all,
    clippy::restriction,
    clippy::pedantic,
    clippy::nursery,
    clippy::cargo
)]

use std::collections::HashMap;
use std::convert::TryFrom;

#[derive(Debug, Clone)]
struct Tuple(Box<(Type, Type)>);
pub fn tuple(a: Type, b: Type) -> Type {
    Type::Tuple(Tuple(Box::new((a,b))))
}

#[derive(Debug, Clone)]
struct Set();

#[derive(Debug, Clone)]
struct Mapping(Box<(Type, Type)>);

#[derive(Debug, Clone)]
enum Type {
    Unit,
    Void,
    Tuple(Tuple),
    Set(Set),
    Mapping(Mapping),
}

impl PartialEq<Type> for Type {
    fn eq(&self, r: &Type) -> bool {
        /// 
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
use super::Type;
use super::tuple;
    #[test]
    fn tuple_assoc() {
        assert_eq!(
            tuple(Type::Void, tuple(Type::Void, Type::Void)),
            tuple(tuple(Type::Void, Type::Void),Type::Void),
            );
    }
}
