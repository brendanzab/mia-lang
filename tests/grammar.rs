extern crate mia;

mod value {
    use mia::Value;

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
    use mia::{StackTerm, Term};
    use mia::Term::*;
    use mia::Value::*;

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
            Ok(Quote(StackTerm::new(vec![Term::call("foo")])))
        );

        assert_eq!(
            "[ 1 2 * + ]".parse(),
            Ok(Quote(StackTerm::new(vec![
                Term::Push(Number(1)),
                Term::Push(Number(2)),
                Term::call("*"),
                Term::call("+"),
            ])))
        );
    }
}

mod stack {
    use mia::{StackTerm, Term};
    use mia::Value::*;

    #[test]
    fn test_compose() {
        assert_eq!("1 2 [ foo ]  * +".parse(),
            Ok(StackTerm::new(vec![
                Term::Push(Number(1)),
                Term::Push(Number(2)),
                Term::quote(vec![Term::call("foo")]),
                Term::call("*"),
                Term::call("+"),
            ])));
        assert_eq!("1 2 [ foo [ -23 bar ] ]  * +".parse(),
            Ok(StackTerm::new(vec![
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

mod var {
    use mia::kind::Var;

    #[test]
    fn test_var() {
        assert_eq!("a".parse(), Ok(Var::new("a")));
        assert_eq!("abc".parse(), Ok(Var::new("abc")));
    }
}

mod stack_var {
    use mia::kind::StackVar;

    #[test]
    fn test_var() {
        assert_eq!("A".parse(), Ok(StackVar::new("A")));
        assert_eq!("ABC".parse(), Ok(StackVar::new("ABC")));
    }
}

mod ty {
    use mia::kind::{StackTy, Ty};
    use mia::kind::{StackVar, Var};

    #[test]
    fn test_bool() {
        assert_eq!("bool".parse(), Ok(Ty::Bool));
    }

    #[test]
    fn test_number() {
        assert_eq!("num".parse(), Ok(Ty::Number));
    }

    #[test]
    fn test_var() {
        assert_eq!("a".parse(), Ok(Ty::Var(Var::new("a"))));
    }

    #[test]
    fn test_fun_id() {
        assert_eq!(
            "(A -> A)".parse(),
            Ok(Ty::Fun(
                StackTy::new(StackVar::new("A"), vec![]),
                StackTy::new(StackVar::new("A"), vec![]),
            ))
        );
    }

    #[test]
    fn test_fun_with_bool() {
        assert_eq!(
            "(A bool -> A)".parse(),
            Ok(Ty::Fun(
                StackTy::new(StackVar::new("A"), vec![Ty::Bool]),
                StackTy::new(StackVar::new("A"), vec![]),
            ))
        );
    }

    #[test]
    fn test_swap() {
        assert_eq!(
            "(A b c -> A c b)".parse(),
            Ok(Ty::Fun(
                StackTy::new(
                    StackVar::new("A"),
                    vec![
                        Ty::Var(Var::new("b")),
                        Ty::Var(Var::new("c")),
                    ],
                ),
                StackTy::new(
                    StackVar::new("A"),
                    vec![
                        Ty::Var(Var::new("c")),
                        Ty::Var(Var::new("b")),
                    ],
                ),
            ))
        );
    }

    #[test]
    fn test_fun_dup() {
        assert_eq!(
            "(A b -> A b b)".parse(),
            Ok(Ty::Fun(
                StackTy::new(
                    StackVar::new("A"),
                    vec![
                        Ty::Var(Var::new("b")),
                    ],
                ),
                StackTy::new(
                    StackVar::new("A"),
                    vec![
                        Ty::Var(Var::new("b")),
                        Ty::Var(Var::new("b")),
                    ],
                ),
            ))
        );
    }
}

mod stack_ty {
    use mia::kind::{StackTy, Ty};
    use mia::kind::{StackVar, Var};

    #[test]
    fn test_empty() {
        assert_eq!(
            "A".parse(),
            Ok(StackTy::new(StackVar::new("A"), vec![]))
        );
    }

    #[test]
    fn test_nonempty() {
        assert_eq!(
            "A bool a".parse(),
            Ok(StackTy::new(
                StackVar::new("A"),
                vec![
                    Ty::Bool,
                    Ty::Var(Var::new("a")),
                ],
            ))
        );
        assert_eq!(
            "A (B -> B)".parse(),
            Ok(StackTy::new(
                StackVar::new("A"),
                vec![
                    Ty::Fun(
                        StackTy::new(StackVar::new("B"), vec![]),
                        StackTy::new(StackVar::new("B"), vec![]),
                    ),
                ],
            ))
        );
    }
}
