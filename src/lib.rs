extern crate itertools;

use itertools::Itertools;

use std::collections::HashMap;
use std::fmt;
use std::str::FromStr;
use std::vec;

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

// This wrapper struct is needed to allow for the derivation of traits on `Term`
#[derive(Copy)]
pub struct Prim {
    pub f: fn(&mut Evaluator) -> EvalResult<()>,
}

impl fmt::Debug for Prim {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Prim(<prim>)")
    }
}

impl Clone for Prim {
    fn clone(&self) -> Prim { *self }
}

impl PartialEq for Prim {
    fn eq(&self, _: &Prim) -> bool {
        unimplemented!() // TODO: figure out function equality
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    Number(i32),
    Quote(Stack),
    Name(String),
    Prim(Prim),
}

impl Term {
    pub fn prim(f: fn(&mut Evaluator) -> EvalResult<()>) -> Term {
        Term::Prim(Prim { f: f })
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
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

        words.define("dup", Term::prim(prim::dup)); // (A b → A b b)
        words.define("pop", Term::prim(prim::pop)); // (A b → A)
        words.define("swap", Term::prim(prim::swap)); // (A b c → A c b)
        words.define("apply", Term::prim(prim::apply)); // (A (A → B) → B)
        words.define("quote", Term::prim(prim::quote)); // (A b → A (C → C b))
        words.define("compose", Term::prim(prim::compose)); // (A (B → C) (C → D) → A (B → D)))

        words.define("+", Term::prim(prim::add)); // (A num num → A num)
        words.define("-", Term::prim(prim::sub)); // (A num num → A num)
        words.define("*", Term::prim(prim::mul)); // (A num num → A num)
        words.define("/", Term::prim(prim::div)); // (A num num → A num)
        words.define("%", Term::prim(prim::rem)); // (A num num → A num)

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

    pub fn quote(&mut self) -> EvalResult<()> {
        let term = try!(self.pop());
        self.push(Term::Quote(Stack::new(vec![term])));
        Ok(())
    }

    fn compose(&mut self, other: Stack) {
        self.terms.extend(other.terms);
    }

    fn dup(&mut self) -> EvalResult<()> {
        let term = try!(self.peek()).clone();
        self.push(term);
        Ok(())
    }

    fn swap(&mut self) -> EvalResult<()> {
        let term_a = try!(self.pop());
        let term_b = try!(self.pop());

        self.push(term_a);
        self.push(term_b);

        Ok(())
    }

    fn apply_binop<F: Fn(i32, i32) -> i32>(&mut self, f: F) -> EvalResult<()> {
        let x = try!(self.pop_number());
        let y = try!(self.pop_number());

        self.push(Term::Number(f(x, y)));

        Ok(())
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

pub struct Evaluator<'a> {
    words: &'a Words,
    stack: Stack,
    quote: Terms,
}

impl<'a> Evaluator<'a> {
    pub fn new(words: &Words, quote: Stack) -> Evaluator {
        Evaluator {
            words: words,
            stack: Stack::new(vec![]),
            quote: quote.into_iter(),

        }
    }

    fn eval_name(&mut self, name: String) -> EvalResult<()> {
        match self.words.lookup(&name) {
            Some(term) => self.eval_term(term.clone()),
            None => Err(EvalError::NameNotFound(name)),
        }
    }

    fn eval_term(&mut self, term: Term) -> EvalResult<()> {
        match term {
            Term::Number(x) => { self.stack.push(Term::Number(x)); Ok(()) },
            Term::Quote(stack) => { self.stack.push(Term::Quote(stack)); Ok(()) },
            Term::Name(name) => self.eval_name(name),
            Term::Prim(prim) => (prim.f)(self),
        }
    }

    fn eval_quote(&mut self, quote: Stack) -> EvalResult<()> {
        let mut quote = quote.into_iter();
        while let Some(term) = quote.next() {
            try!(self.eval_term(term))
        }
        Ok(())
    }

    fn eval_stack(&mut self) -> EvalResult<()> {
        while let Some(term) = self.quote.next() {
            try!(self.eval_term(term))
        }
        Ok(())
    }

    pub fn eval(mut self) -> EvalResult<Stack> {
        try!(self.eval_stack());
        Ok(self.stack)
    }
}

mod prim {
    use std::ops::*;
    use super::*;

    // dup : (A b → A b b)
    pub fn dup(ev: &mut Evaluator) -> EvalResult<()> {
        ev.stack.dup()
    }

    // pop : (A b → A)
    pub fn pop(ev: &mut Evaluator) -> EvalResult<()> {
        ev.stack.pop().map(|_| ())
    }

    // swap : (A b c → A c b)
    pub fn swap(ev: &mut Evaluator) -> EvalResult<()> {
        ev.stack.swap()
    }

    // apply : (A (A → B) → B)
    pub fn apply(ev: &mut Evaluator) -> EvalResult<()> {
        let quote = try!(ev.stack.pop_quote());

        ev.eval_quote(quote)
    }

    // quote : (A b → A (C → C b))
    pub fn quote(ev: &mut Evaluator) -> EvalResult<()> {
        let term = try!(ev.stack.pop());
        let quoted = Term::Quote(Stack::new(vec![term]));

        ev.stack.push(quoted);

        Ok(())
    }

    // compose : (A (B → C) (C → D) → A (B → D)))
    pub fn compose(ev: &mut Evaluator) -> EvalResult<()> {
        let stack_a = try!(ev.stack.pop_quote());
        let mut stack_b = try!(ev.stack.pop_quote());

        stack_b.compose(stack_a);
        ev.stack.push(Term::Quote(stack_b));

        Ok(())
    }

    // add : (A num num → A num)
    pub fn add(ev: &mut Evaluator) -> EvalResult<()> {
        ev.stack.apply_binop(i32::add)
    }

    // sub : (A num num → A num)
    pub fn sub(ev: &mut Evaluator) -> EvalResult<()> {
        ev.stack.apply_binop(i32::sub)
    }

    // mul : (A num num → A num)
    pub fn mul(ev: &mut Evaluator) -> EvalResult<()> {
        ev.stack.apply_binop(i32::mul)
    }

    // div : (A num num → A num)
    pub fn div(ev: &mut Evaluator) -> EvalResult<()> {
        ev.stack.apply_binop(i32::div)
    }

    // rem : (A num num → A num)
    pub fn rem(ev: &mut Evaluator) -> EvalResult<()> {
        ev.stack.apply_binop(i32::rem)
    }
}
