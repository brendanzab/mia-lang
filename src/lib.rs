extern crate itertools;

use itertools::Itertools;

use std::collections::HashMap;
use std::fmt;
use std::str::FromStr;
use std::vec;

mod prim;

#[derive(Debug, Clone)]
pub enum EvalError {
    StackUnderflow,
    NameNotFound(String),
    TypeMismatch,
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            EvalError::StackUnderflow => write!(f, "stack underflow"),
            EvalError::NameNotFound(ref name) => write!(f, "could not find name: {}", name),
            EvalError::TypeMismatch => write!(f, "type mismatch"),
        }
    }
}

pub type EvalResult<T> = Result<T, EvalError>;

// The type `fn(&mut T, &mut V) -> V` does not implement `Debug`, `Clone`, or
// `PartialEq`, so to enable `#[derive(..)]` to work for `Term`, we implement
// these for a wrapper struct.
#[derive(Copy)]
pub struct Prim(pub fn(&mut Stack, &Words) -> EvalResult<()>);

impl fmt::Debug for Prim {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Prim(..)")
    }
}

impl Clone for Prim {
    fn clone(&self) -> Prim { Prim(self.0) }
}

impl PartialEq for Prim {
    fn eq(&self, other: &Prim) -> bool {
        &self.0 as *const _ == &other.0 as *const _
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    Bool(bool),
    Number(i32),
    Quote(Stack),
    Name(String),
    Prim(Prim),
}

impl Term {
    pub fn prim(f: fn(&mut Stack, &Words) -> EvalResult<()>) -> Term {
        Term::Prim(Prim(f))
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Term::Bool(true) => write!(f, "true"),
            Term::Bool(false) => write!(f, "false"),
            Term::Number(x) => write!(f, "{}", x),
            Term::Quote(ref stack) => write!(f, "[ {} ]", stack),
            Term::Name(ref name) => write!(f, "{}", name),
            Term::Prim(_) => write!(f, "<prim>"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Words {
    defs: HashMap<String, Term>,
}

impl Words {
    pub fn empty() -> Words {
        Words {
            defs: HashMap::new(),
        }
    }

    pub fn standard() -> Words {
        let mut words = Words::empty();

        words.define("words", Term::prim(prim::words)); // (A ~> A)

        words.define("dup", Term::prim(prim::dup)); // (A b -> A b b)
        words.define("pop", Term::prim(prim::pop)); // (A b -> A)
        words.define("swap", Term::prim(prim::swap)); // (A b c -> A c b)
        words.define("apply", Term::prim(prim::apply)); // (A (A -> B) -> B)
        words.define("quote", Term::prim(prim::quote)); // (A b -> A (C -> C b))
        words.define("compose", Term::prim(prim::compose)); // (A (B -> C) (C -> D) -> A (B -> D)))

        words.define("if", Term::prim(prim::if_)); // (A bool (A -> B) (A -> B) -> B)

        words.define("true", Term::Bool(true)); // bool
        words.define("false", Term::Bool(false)); // bool

        words.define("eq", Term::prim(prim::eq)); // (A num num -> A bool)
        words.define("and", Term::prim(prim::and)); // (A bool bool -> A bool)
        words.define("or", Term::prim(prim::or)); // (A bool bool -> A bool)
        words.define("not", Term::prim(prim::not)); // (A bool -> A bool)

        words.define("+", Term::prim(prim::add)); // (A num num -> A num)
        words.define("-", Term::prim(prim::sub)); // (A num num -> A num)
        words.define("*", Term::prim(prim::mul)); // (A num num -> A num)
        words.define("/", Term::prim(prim::div)); // (A num num -> A num)
        words.define("%", Term::prim(prim::rem)); // (A num num -> A num)

        words
    }

    pub fn define<S: Into<String>>(&mut self, name: S, x: Term) {
        self.defs.insert(name.into(), x);
    }

    pub fn lookup(&self, name: &str) -> Option<&Term> {
        self.defs.get(name)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Stack {
    terms: Vec<Term>,
}

impl Stack {
    pub fn new(terms: Vec<Term>) -> Stack {
        Stack { terms: terms }
    }

    fn push(&mut self, term: Term) {
        self.terms.push(term);
    }

    fn pop(&mut self) -> EvalResult<Term> {
        match self.terms.pop() {
            Some(term) => Ok(term),
            None => Err(EvalError::StackUnderflow),
        }
    }

    fn pop_bool(&mut self) -> EvalResult<bool> {
        match try!(self.pop()) {
            Term::Bool(x) => Ok(x),
            _ => Err(EvalError::TypeMismatch),
        }
    }

    fn pop_number(&mut self) -> EvalResult<i32> {
        match try!(self.pop()) {
            Term::Number(x) => Ok(x),
            _ => Err(EvalError::TypeMismatch),
        }
    }

    fn pop_quote(&mut self) -> EvalResult<Stack> {
        match try!(self.pop()) {
            Term::Quote(stack) => Ok(stack),
            _ => Err(EvalError::TypeMismatch),
        }
    }

    fn peek(&mut self) -> EvalResult<&Term> {
        match self.terms.last() {
            Some(term) => Ok(term),
            None => Err(EvalError::StackUnderflow),
        }
    }

    fn eval_name(&mut self, words: &Words, name: String) -> EvalResult<()> {
        match words.lookup(&name) {
            Some(term) => self.eval_term(words, term.clone()),
            None => Err(EvalError::NameNotFound(name)),
        }
    }

    fn eval_term(&mut self, words: &Words, term: Term) -> EvalResult<()> {
        match term {
            Term::Bool(x) => { self.push(Term::Bool(x)); Ok(()) },
            Term::Number(x) => { self.push(Term::Number(x)); Ok(()) },
            Term::Quote(stack) => { self.push(Term::Quote(stack)); Ok(()) },
            Term::Name(name) => self.eval_name(words, name),
            Term::Prim(Prim(f)) => f(self, words),
        }
    }

    fn apply(&mut self, words: &Words, quote: Stack) -> EvalResult<()> {
        let mut quote = quote.into_iter();
        while let Some(term) = quote.next() {
            try!(self.eval_term(words, term))
        }
        Ok(())
    }
}

pub fn eval(stack: Stack, words: &Words) -> EvalResult<Stack> {
    let mut result = Stack::new(vec![]);
    try!(result.apply(words, stack));
    Ok(result)
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
                        Ok(x) => terms.push(Term::Number(x)),
                        Err(_) => terms.push(Term::Name(token.to_string())),
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

pub type Terms = vec::IntoIter<Term>;

impl IntoIterator for Stack {
    type Item = Term;
    type IntoIter = Terms;

    fn into_iter(self) -> Terms {
        self.terms.into_iter()
    }
}

impl fmt::Display for Stack {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.terms.iter().format(" ", |t, f| f(t)))
    }
}

#[cfg(test)]
mod tests {
    mod prim {
        mod eq {
            use Prim;
            use prim;

            #[test]
            fn test_same() {
                assert_eq!(Prim(prim::eq), Prim(prim::eq));
                assert_eq!(Prim(prim::add), Prim(prim::add));
            }

            #[test]
            fn test_different() {
                assert!(Prim(prim::eq) != Prim(prim::add));
                assert!(Prim(prim::add) != Prim(prim::eq));
            }
        }
    }

    mod stack {
        mod parse {
            use Stack;
            use Term::*;

            #[test]
            fn test_number() {
                assert_eq!("123".parse(), Ok(Stack::new(vec![Number(123)])));
                assert_eq!(" 34 ".parse(), Ok(Stack::new(vec![Number(34)])));
            }

            #[test]
            fn test_name() {
                assert_eq!("foo".parse(), Ok(Stack::new(vec![Name("foo".to_string())])));
                assert_eq!(" * ".parse(), Ok(Stack::new(vec![Name("*".to_string())])));
            }

            #[test]
            fn test_quote() {
                assert_eq!("[ foo ]".parse(),
                    Ok(Stack::new(vec![
                        Quote(Stack::new(vec![Name("foo".to_string())]))
                    ])));

                assert_eq!("[ 1 2 * + ]".parse(),
                    Ok(Stack::new(vec![
                        Quote(Stack::new(vec![
                            Number(1),
                            Number(2),
                            Name("*".to_string()),
                            Name("+".to_string()),
                        ])),
                    ])));
            }

            #[test]
            fn test_compose() {
                assert_eq!(" 1 2 [ foo ]  * +".parse(),
                    Ok(Stack::new(vec![
                        Number(1),
                        Number(2),
                        Quote(Stack::new(vec![Name("foo".to_string())])),
                        Name("*".to_string()),
                        Name("+".to_string()),
                    ])));
            }
        }
    }
}
