use std::str::FromStr;

use {Stack, Term, Value};

impl FromStr for Value {
    type Err = String;

    fn from_str(src: &str) -> Result<Value, String> {
        if let Ok(x) = src.parse() {
            return Ok(Value::Bool(x));
        }
        if let Ok(x) = src.parse() {
            return Ok(Value::Number(x));
        }

        Err("Expected a boolean or number".to_string())
    }
}

impl FromStr for Stack {
    type Err = String;

    fn from_str(src: &str) -> Result<Stack, String> {
        fn parse_tokens<'a, I>(tokens: &mut I, in_quote: bool) -> Result<Stack, String> where
            I: Iterator<Item = &'a str>,
        {
            let mut terms = vec![];

            while let Some(token) = tokens.next() {
                match token {
                    "[" => {
                        terms.push(Term::Quote(
                            try!(parse_tokens(tokens, true))
                        ))
                    },
                    "]" => {
                        if in_quote {
                            return Ok(Stack::new(terms));
                        } else {
                            return Err("found unexpected `]`".to_string());
                        }
                    },
                    token => match token.parse() {
                        Ok(value) => terms.push(Term::Push(value)),
                        Err(_) => terms.push(Term::Call(token.to_string())),
                    },
                }
            }

            if in_quote {
                Err("expected closing `]`".to_string())
            } else {
                Ok(Stack::new(terms))
            }
        }

        parse_tokens(&mut src.split_whitespace(), false)
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
            assert_eq!("blagh".parse::<Value>(), Err("Expected a boolean or number".to_string()));
            assert_eq!("35.3".parse::<Value>(), Err("Expected a boolean or number".to_string()));
        }
    }

    mod stack {
        use {Stack, Term};
        use Term::*;
        use Value::*;

        #[test]
        fn test_push() {
            assert_eq!("123".parse(), Ok(Stack::new(vec![Term::Push(Number(123))])));
            assert_eq!(" -34 ".parse(), Ok(Stack::new(vec![Term::Push(Number(-34))])));
            assert_eq!("true".parse(), Ok(Stack::new(vec![Term::Push(Bool(true))])));
            assert_eq!(" false ".parse(), Ok(Stack::new(vec![Term::Push(Bool(false))])));
        }

        #[test]
        fn test_call() {
            assert_eq!("foo".parse(), Ok(Stack::new(vec![Term::call("foo")])));
            assert_eq!(" * ".parse(), Ok(Stack::new(vec![Term::call("*")])));
        }

        #[test]
        fn test_quote() {
            assert_eq!("[ foo ]".parse(),
                Ok(Stack::new(vec![
                    Quote(Stack::new(vec![Term::call("foo")]))
                ])));

            assert_eq!("[ 1 2 * + ]".parse(),
                Ok(Stack::new(vec![
                    Quote(Stack::new(vec![
                        Term::Push(Number(1)),
                        Term::Push(Number(2)),
                        Term::call("*"),
                        Term::call("+"),
                    ])),
                ])));
        }

        #[test]
        fn test_compose() {
            assert_eq!(" 1 2 [ foo ]  * +".parse(),
                Ok(Stack::new(vec![
                    Term::Push(Number(1)),
                    Term::Push(Number(2)),
                    Quote(Stack::new(vec![Term::call("foo")])),
                    Term::call("*"),
                    Term::call("+"),
                ])));
            assert_eq!(" 1 2 [ foo [ -23 bar ] ]  * +".parse(),
                Ok(Stack::new(vec![
                    Term::Push(Number(1)),
                    Term::Push(Number(2)),
                    Quote(Stack::new(vec![
                        Term::call("foo"),
                        Quote(Stack::new(vec![
                            Term::Push(Number(-23)),
                            Term::call("bar")
                        ]))
                    ])),
                    Term::call("*"),
                    Term::call("+"),
                ])));
        }
    }
}
