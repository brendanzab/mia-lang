use std::fmt;

use kind::{TypeKind, StackKind};

pub trait KindVar {
    type Kind;

    fn new<S: ToString>(name: S) -> Self;

    fn gen() -> Self;
}

macro_rules! kind_var {
    ($KindVar:ident, $Kind:ident, $prefix:expr) => {
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
    };
}

kind_var!(TypeVar, TypeKind, "t");
kind_var!(StackVar, StackKind, "S");

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
