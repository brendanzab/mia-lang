use itertools::Itertools;

use std::fmt;
use std::marker::PhantomData;

#[derive(Clone, Debug, Hash, PartialEq)]
pub struct Var<Kind>(String, PhantomData<Kind>);

pub type TypeVar = Var<TypeKind>;
pub type StackVar = Var<StackKind>;

impl<Kind> Var<Kind> {
    pub fn new<S: Into<String>>(name: S) -> Var<Kind> {
        Var(name.into(), PhantomData)
    }
}

impl fmt::Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for StackVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

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
            TypeKind::Bool => write!(f, "Bool"),
            TypeKind::Number => write!(f, "Number"),
            TypeKind::Var(ref var) => write!(f, "{}", var),
            TypeKind::Fun(ref a, ref b) => write!(f, "({} -> {})", a, b),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct StackKind(StackVar, Vec<TypeKind>);

impl fmt::Display for StackKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.0, self.1.iter().format(" ", |t, f| f(t)))
    }
}
