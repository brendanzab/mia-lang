use itertools::Itertools;
use std::fmt;
use std::str::FromStr;

use grammar;

pub trait KindVar {
    type Kind;

    fn new<S: ToString>(name: S) -> Self;

    fn gen() -> Self;
}

macro_rules! kind_var {
    ($KindVar:ident, $Kind:ident, $prefix:expr, $parse_var:ident) => {
        #[derive(Clone, Debug, Hash, PartialEq)]
        pub struct $KindVar {
            name: String,
        }

        impl KindVar for $KindVar {
            type Kind = $Kind;

            fn new<S: ToString>(name: S) -> Self {
                $KindVar {
                    name: name.to_string(),
                }
            }

            fn gen() -> $KindVar {
                use std::cell::Cell;

                thread_local!(static NEXT_ID: Cell<usize> = Cell::new(0));

                NEXT_ID.with(|next_id| {
                    let id = next_id.get();
                    next_id.set(id + 1);

                    $KindVar::new(format!("{}${}", $prefix, id))
                })
            }
        }

        impl fmt::Display for $KindVar {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{}", self.name)
            }
        }

        impl FromStr for $KindVar {
            type Err = grammar::ParseError;

            fn from_str(src: &str) -> Result<$KindVar, grammar::ParseError> {
                grammar::$parse_var(src)
            }
        }
    };
}

kind_var!(TypeVar, TypeKind, "t", type_var);
kind_var!(StackVar, StackKind, "S", stack_var);

macro_rules! forall_fn {
    ($name:ident($($Var:ident),*)) => {
        #[allow(dead_code)]
        pub fn $name<F: FnOnce($($Var),*) -> Output, $($Var: KindVar,)* Output>(f: F) -> Output {
            f($($Var::gen()),*)
        }
    }
}

forall_fn!(forall1(Var1));
forall_fn!(forall2(Var1, Var2));
forall_fn!(forall3(Var1, Var2, Var3));
forall_fn!(forall4(Var1, Var2, Var3, Var4));
forall_fn!(forall5(Var1, Var2, Var3, Var4, Var5));


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

impl FromStr for TypeKind {
    type Err = grammar::ParseError;

    fn from_str(src: &str) -> Result<TypeKind, grammar::ParseError> {
        grammar::type_kind(src)
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

impl FromStr for StackKind {
    type Err = grammar::ParseError;

    fn from_str(src: &str) -> Result<StackKind, grammar::ParseError> {
        grammar::stack_kind(src)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_kind_var_gen() {
        assert_eq!(TypeVar::gen().to_string(), "t$0");
        assert_eq!(StackVar::gen().to_string(), "S$0");

        assert_eq!(TypeVar::gen().to_string(), "t$1");
        assert_eq!(StackVar::gen().to_string(), "S$1");

        assert_eq!(TypeVar::gen().to_string(), "t$2");
        assert_eq!(StackVar::gen().to_string(), "S$2");
    }
}
