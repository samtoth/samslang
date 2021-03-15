use std::fmt::Debug;

/// SyntaxNode
///
#[derive(Debug)]
pub enum SyntaxNode {
    Statement,
    Expression,
    Tuple,
    Function,
    VariableRef,
    VariableDef,
    Set,
    Predicate,
}

#[cfg(test)]
mod tests {}
