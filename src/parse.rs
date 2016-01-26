use std::str::FromStr;

use {Stack, Term, Value};
use kind::{TypeKind, StackKind};
use kind_var::{TypeVar, StackVar};

peg_file! grammar("grammar.rustpeg");

impl FromStr for Value {
    type Err = grammar::ParseError;

    fn from_str(src: &str) -> Result<Value, grammar::ParseError> {
        grammar::value(src)
    }
}

impl FromStr for Term {
    type Err = grammar::ParseError;

    fn from_str(src: &str) -> Result<Term, grammar::ParseError> {
        grammar::term(src)
    }
}

impl FromStr for Stack {
    type Err = grammar::ParseError;

    fn from_str(src: &str) -> Result<Stack, grammar::ParseError> {
        grammar::stack(src)
    }
}

impl FromStr for TypeVar {
    type Err = grammar::ParseError;

    fn from_str(src: &str) -> Result<TypeVar, grammar::ParseError> {
        grammar::type_var(src)
    }
}

impl FromStr for StackVar {
    type Err = grammar::ParseError;

    fn from_str(src: &str) -> Result<StackVar, grammar::ParseError> {
        grammar::stack_var(src)
    }
}

impl FromStr for TypeKind {
    type Err = grammar::ParseError;

    fn from_str(src: &str) -> Result<TypeKind, grammar::ParseError> {
        grammar::type_kind(src)
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
    mod value {
        use Value;

        #[test]
        fn test_bool() {
            assert_eq!("true".parse(), Ok(Value::Bool(true)));
            assert_eq!("false".parse(), Ok(Value::Bool(false)));
        }

        #[test]
        fn test_number() {
            assert_eq!("123".parse(), Ok(Value::Number(123)));
            assert_eq!("-34".parse(), Ok(Value::Number(-34)));
        }

        #[test]
        fn test_err() {
            assert!("blagh".parse::<Value>().is_err());
            assert!("35.3".parse::<Value>().is_err());
        }
    }

    mod term {
        use {Stack, Term};
        use Term::*;
        use Value::*;

        #[test]
        fn test_push() {
            assert_eq!("123".parse(), Ok(Term::Push(Number(123))));
            assert_eq!("-34".parse(), Ok(Term::Push(Number(-34))));
            assert_eq!("true".parse(), Ok(Term::Push(Bool(true))));
            assert_eq!("false".parse(), Ok(Term::Push(Bool(false))));
        }

        #[test]
        fn test_call() {
            assert_eq!("foo".parse(), Ok(Term::call("foo")));
            assert_eq!("*".parse(), Ok(Term::call("*")));
        }

        #[test]
        fn test_quote_empty() {
            assert_eq!(
                "[ ]".parse(),
                Ok(Term::quote(vec![]))
            );
            assert_eq!(
                "[  [  ] [ ] ]".parse(),
                Ok(Term::quote(vec![
                    Term::quote(vec![]),
                    Term::quote(vec![]),
                ]))
            );
        }

        #[test]
        fn test_quote() {
            assert_eq!(
                "[ foo ]".parse(),
                Ok(Quote(Stack::new(vec![Term::call("foo")])))
            );

            assert_eq!(
                "[ 1 2 * + ]".parse(),
                Ok(Quote(Stack::new(vec![
                    Term::Push(Number(1)),
                    Term::Push(Number(2)),
                    Term::call("*"),
                    Term::call("+"),
                ])))
            );
        }
    }

    mod stack {
        use {Stack, Term};
        use Value::*;

        #[test]
        fn test_compose() {
            assert_eq!("1 2 [ foo ]  * +".parse(),
                Ok(Stack::new(vec![
                    Term::Push(Number(1)),
                    Term::Push(Number(2)),
                    Term::quote(vec![Term::call("foo")]),
                    Term::call("*"),
                    Term::call("+"),
                ])));
            assert_eq!("1 2 [ foo [ -23 bar ] ]  * +".parse(),
                Ok(Stack::new(vec![
                    Term::Push(Number(1)),
                    Term::Push(Number(2)),
                    Term::quote(vec![
                        Term::call("foo"),
                        Term::quote(vec![
                            Term::Push(Number(-23)),
                            Term::call("bar")
                        ])
                    ]),
                    Term::call("*"),
                    Term::call("+"),
                ])));
        }
    }

    mod type_var {
        use kind_var::{KindVar, TypeVar};

        #[test]
        fn test_var() {
            assert_eq!("'a".parse(), Ok(TypeVar::new("a")));
            assert_eq!("'abc".parse(), Ok(TypeVar::new("abc")));
        }
    }

    mod stack_var {
        use kind_var::{KindVar, StackVar};

        #[test]
        fn test_var() {
            assert_eq!("'A".parse(), Ok(StackVar::new("A")));
            assert_eq!("'ABC".parse(), Ok(StackVar::new("ABC")));
        }
    }

    mod type_kind {
        use kind::{StackKind, TypeKind};
        use kind_var::{KindVar, StackVar, TypeVar};

        #[test]
        fn test_bool() {
            assert_eq!("bool".parse(), Ok(TypeKind::Bool));
        }

        #[test]
        fn test_number() {
            assert_eq!("num".parse(), Ok(TypeKind::Number));
        }

        #[test]
        fn test_var() {
            assert_eq!("'a".parse(), Ok(TypeKind::Var(TypeVar::new("a"))));
        }

        #[test]
        fn test_fun_id() {
            assert_eq!(
                "('A -> 'A)".parse(),
                Ok(TypeKind::Fun(
                    StackKind::new(StackVar::new("A"), vec![]),
                    StackKind::new(StackVar::new("A"), vec![]),
                ))
            );
        }

        #[test]
        fn test_fun_with_bool() {
            assert_eq!(
                "('A bool -> 'A)".parse(),
                Ok(TypeKind::Fun(
                    StackKind::new(StackVar::new("A"), vec![TypeKind::Bool]),
                    StackKind::new(StackVar::new("A"), vec![]),
                ))
            );
        }

        #[test]
        fn test_swap() {
            assert_eq!(
                "('A 'b 'c -> 'A 'c 'b)".parse(),
                Ok(TypeKind::Fun(
                    StackKind::new(
                        StackVar::new("A"),
                        vec![
                            TypeKind::Var(TypeVar::new("b")),
                            TypeKind::Var(TypeVar::new("c")),
                        ],
                    ),
                    StackKind::new(
                        StackVar::new("A"),
                        vec![
                            TypeKind::Var(TypeVar::new("c")),
                            TypeKind::Var(TypeVar::new("b")),
                        ],
                    ),
                ))
            );
        }

        #[test]
        fn test_fun_dup() {
            assert_eq!(
                "('A 'b -> 'A 'b 'b)".parse(),
                Ok(TypeKind::Fun(
                    StackKind::new(
                        StackVar::new("A"),
                        vec![
                            TypeKind::Var(TypeVar::new("b")),
                        ],
                    ),
                    StackKind::new(
                        StackVar::new("A"),
                        vec![
                            TypeKind::Var(TypeVar::new("b")),
                            TypeKind::Var(TypeVar::new("b")),
                        ],
                    ),
                ))
            );
        }
    }

    mod stack_kind {
        use kind::{StackKind, TypeKind};
        use kind_var::{KindVar, StackVar, TypeVar};

        #[test]
        fn test_empty() {
            assert_eq!(
                "'A".parse(),
                Ok(StackKind::new(StackVar::new("A"), vec![]))
            );
        }

        #[test]
        fn test_nonempty() {
            assert_eq!(
                "'A bool 'a".parse(),
                Ok(StackKind::new(
                    StackVar::new("A"),
                    vec![
                        TypeKind::Bool,
                        TypeKind::Var(TypeVar::new("a")),
                    ],
                ))
            );
            assert_eq!(
                "'A ('B -> 'B)".parse(),
                Ok(StackKind::new(
                    StackVar::new("A"),
                    vec![
                        TypeKind::Fun(
                            StackKind::new(StackVar::new("B"), vec![]),
                            StackKind::new(StackVar::new("B"), vec![]),
                        ),
                    ],
                ))
            );
        }
    }
}
