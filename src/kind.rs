use itertools::Itertools;
use std::fmt;
use std::str::FromStr;

use grammar;

pub trait GenVar {
    type Kind;

    fn gen() -> Self;
}

macro_rules! kind_var {
    ($KindVar:ident, $Kind:ident, $prefix:expr, $parse_var:ident) => {
        #[derive(Clone, Debug, Hash, PartialEq)]
        pub struct $KindVar {
            name: String,
        }

        impl $KindVar {
            pub fn new<S: ToString>(name: S) -> Self {
                $KindVar {
                    name: name.to_string(),
                }
            }
        }

        impl GenVar for $KindVar {
            type Kind = $Kind;

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

kind_var!(Var, Ty, "t", var);
kind_var!(StackVar, StackTy, "S", stack_var);

macro_rules! forall_fn {
    ($name:ident($($Var:ident),*)) => {
        #[allow(dead_code)]
        pub fn $name<F: FnOnce($($Var),*) -> Output, $($Var: GenVar,)* Output>(f: F) -> Output {
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
pub struct Fun(pub StackTy, pub StackTy);

impl fmt::Display for Fun {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} -> {}", self.0, self.1)
    }
}

impl FromStr for Fun {
    type Err = grammar::ParseError;

    fn from_str(src: &str) -> Result<Fun, grammar::ParseError> {
        grammar::fun(src)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Ty {
    Bool,
    Number,
    Var(Var),
    Fun(Fun),
}

impl Ty {
    pub fn fun(lhs: StackTy, rhs: StackTy) -> Ty {
        Ty::Fun(Fun(lhs, rhs))
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Ty::Bool => write!(f, "bool"),
            Ty::Number => write!(f, "num"),
            Ty::Var(ref var) => write!(f, "{}", var),
            Ty::Fun(ref fun) => write!(f, "({})", fun),
        }
    }
}

impl FromStr for Ty {
    type Err = grammar::ParseError;

    fn from_str(src: &str) -> Result<Ty, grammar::ParseError> {
        grammar::ty(src)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct StackTy {
    pub var: StackVar,
    pub tys: Vec<Ty>,
}

impl StackTy {
    pub fn new(var: StackVar, tys: Vec<Ty>) -> StackTy {
        StackTy { var: var, tys: tys }
    }
}

impl fmt::Display for StackTy {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.var, self.tys.iter().format(" ", |t, f| f(t)))
    }
}

impl FromStr for StackTy {
    type Err = grammar::ParseError;

    fn from_str(src: &str) -> Result<StackTy, grammar::ParseError> {
        grammar::stack_ty(src)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_kind_var_gen() {
        assert_eq!(Var::gen().to_string(), "t$0");
        assert_eq!(StackVar::gen().to_string(), "S$0");

        assert_eq!(Var::gen().to_string(), "t$1");
        assert_eq!(StackVar::gen().to_string(), "S$1");

        assert_eq!(Var::gen().to_string(), "t$2");
        assert_eq!(StackVar::gen().to_string(), "S$2");
    }
}
