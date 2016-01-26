use std::str::FromStr;

use {Stack, Term, Value};

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
}
