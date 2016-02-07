#![feature(plugin)]
#![plugin(peg_syntax_ext)]

extern crate itertools;

use itertools::Itertools;
use std::collections::HashMap;
use std::fmt;
use std::str::FromStr;

use kind::Ty;

pub mod kind;
pub mod prim;

peg_file! grammar("grammar.rustpeg");

#[derive(Debug, Clone)]
pub enum EvalError {
    StackUnderflow,
    NotFound(String),
    TypeMismatch,
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            EvalError::StackUnderflow => write!(f, "stack underflow"),
            EvalError::NotFound(ref name) => write!(f, "could not find `{}`", name),
            EvalError::TypeMismatch => write!(f, "type mismatch"),
        }
    }
}

pub type EvalResult = Result<StackTerm, EvalError>;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Value {
    Bool(bool),
    Number(i32),
}

impl FromStr for Value {
    type Err = grammar::ParseError;

    fn from_str(src: &str) -> Result<Value, grammar::ParseError> {
        grammar::value(src)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Bool(x) => write!(f, "{}", x),
            Value::Number(x) => write!(f, "{}", x),
        }
    }
}

pub type PrimDef = fn(StackTerm, &Words) -> EvalResult;

pub struct Prim {
    pub ty: Ty,
    pub def: PrimDef,
}

impl Prim {
    pub fn new(ty: &str, def: PrimDef) -> Prim {
        Prim {
            ty: ty.parse().unwrap(),
            def: def,
        }
    }

    pub fn call(self, stack_term: StackTerm, words: &Words) -> EvalResult {
        (self.def)(stack_term, words)
    }
}

impl fmt::Debug for Prim {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Prim({:#x})", self.def as usize)
    }
}

impl Clone for Prim {
    fn clone(&self) -> Prim { Prim { ty: self.ty.clone(), def: self.def } }
}

impl PartialEq for Prim {
    fn eq(&self, other: &Prim) -> bool {
        self.def as usize == other.def as usize && self.ty == other.ty
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    Push(Value),
    Quote(StackTerm),
    Call(String),
    Prim(Prim),
}

impl Term {
    pub fn call<S: ToString>(name: S) -> Term {
        Term::Call(name.to_string())
    }

    pub fn quote(terms: Vec<Term>) -> Term {
        Term::Quote(StackTerm::new(terms))
    }
}

impl FromStr for Term {
    type Err = grammar::ParseError;

    fn from_str(src: &str) -> Result<Term, grammar::ParseError> {
        grammar::term(src)
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Term::Push(value) => write!(f, "{}", value),
            Term::Quote(ref stack_term) if stack_term.terms.is_empty() => write!(f, "[ ]"),
            Term::Quote(ref stack_term) => write!(f, "[ {} ]", stack_term),
            Term::Call(ref name) => write!(f, "{}", name),
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

    pub fn std() -> Words {
        let mut words = Words::empty();

        words.define("words", Term::Prim(prim::words()));

        words.define("dup", Term::Prim(prim::dup()));
        words.define("pop", Term::Prim(prim::pop()));
        words.define("swap", Term::Prim(prim::swap()));
        words.define("apply", Term::Prim(prim::apply()));
        words.define("quote", Term::Prim(prim::quote()));
        words.define("compose", Term::Prim(prim::compose()));

        words.define("if", Term::Prim(prim::if_()));

        words.define("eq", Term::Prim(prim::eq()));
        words.define("and", Term::Prim(prim::and()));
        words.define("or", Term::Prim(prim::or()));
        words.define("not", Term::Prim(prim::not()));

        words.define("+", Term::Prim(prim::add()));
        words.define("-", Term::Prim(prim::sub()));
        words.define("*", Term::Prim(prim::mul()));
        words.define("/", Term::Prim(prim::div()));
        words.define("%", Term::Prim(prim::rem()));

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
pub struct StackTerm {
    terms: Vec<Term>,
}

impl StackTerm {
    pub fn new(terms: Vec<Term>) -> StackTerm {
        StackTerm { terms: terms }
    }

    pub fn empty() -> StackTerm {
        StackTerm::new(vec![])
    }

    fn push(mut self, term: Term) -> StackTerm {
        self.terms.push(term);
        self
    }

    fn pop(mut self) -> Result<(StackTerm, Term), EvalError> {
        match self.terms.pop() {
            Some(term) => Ok((self, term)),
            None => Err(EvalError::StackUnderflow),
        }
    }

    fn pop_bool(self) -> Result<(StackTerm, bool), EvalError> {
        let (stack_term, term) = try!(self.pop());
        match term {
            Term::Push(Value::Bool(x)) => Ok((stack_term, x)),
            _ => Err(EvalError::TypeMismatch),
        }
    }

    fn pop_number(self) -> Result<(StackTerm, i32), EvalError> {
        let (stack_term, term) = try!(self.pop());
        match term {
            Term::Push(Value::Number(x)) => Ok((stack_term, x)),
            _ => Err(EvalError::TypeMismatch),
        }
    }

    fn pop_quote(self) -> Result<(StackTerm, StackTerm), EvalError> {
        let (stack_term, term) = try!(self.pop());
        match term {
            Term::Quote(quoted) => Ok((stack_term, quoted)),
            _ => Err(EvalError::TypeMismatch),
        }
    }

    fn peek(&self) -> Result<&Term, EvalError> {
        match self.terms.last() {
            Some(term) => Ok(term),
            None => Err(EvalError::StackUnderflow),
        }
    }

    fn eval_term(self, words: &Words, term: Term) -> EvalResult {
        match term {
            Term::Push(value) => Ok(self.push(Term::Push(value))),
            Term::Quote(stack_term) => Ok(self.push(Term::Quote(stack_term))),
            Term::Call(name) => {
                match words.lookup(&name) {
                    Some(term) => self.eval_term(words, term.clone()),
                    None => Err(EvalError::NotFound(name)),
                }
            },
            Term::Prim(prim) => prim.call(self, words),
        }
    }

    fn eval_stack(mut self, words: &Words, quote: StackTerm) -> EvalResult {
        let mut terms = quote.terms.into_iter();
        while let Some(term) = terms.next() {
            self = try!(self.eval_term(words, term));
        }
        Ok(self)
    }
}

pub fn eval(stack_term: StackTerm, words: &Words) -> EvalResult {
    StackTerm::empty().eval_stack(words, stack_term)
}

impl FromStr for StackTerm {
    type Err = grammar::ParseError;

    fn from_str(src: &str) -> Result<StackTerm, grammar::ParseError> {
        grammar::stack_term(src)
    }
}

impl fmt::Display for StackTerm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.terms.iter().format(" ", |t, f| f(t)))
    }
}

#[cfg(test)]
mod tests {
    mod prim {
        mod eq {
            use prim;

            #[test]
            fn test_same() {
                assert_eq!(prim::eq(), prim::eq());
                assert_eq!(prim::add(), prim::add());
            }

            #[test]
            fn test_different() {
                assert!(prim::eq() != prim::add());
                assert!(prim::add() != prim::eq());
            }
        }
    }
}
