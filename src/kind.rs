use itertools::Itertools;

use std::fmt;

use kind_var::{TypeVar, StackVar};

#[derive(Clone, Debug, PartialEq)]
pub enum TypeKind {
    Bool,
    Number,
    Var(TypeVar),
    Fun(StackKind, StackKind),
}

impl fmt::Display for TypeKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TypeKind::Bool => write!(f, "bool"),
            TypeKind::Number => write!(f, "num"),
            TypeKind::Var(ref var) => write!(f, "{}", var),
            TypeKind::Fun(ref a, ref b) => write!(f, "({} -> {})", a, b),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct StackKind {
    pub var: StackVar,
    pub tys: Vec<TypeKind>,
}

impl StackKind {
    pub fn new(var: StackVar, tys: Vec<TypeKind>) -> StackKind {
        StackKind { var: var, tys: tys }
    }
}

impl fmt::Display for StackKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.var, self.tys.iter().format(" ", |t, f| f(t)))
    }
}
