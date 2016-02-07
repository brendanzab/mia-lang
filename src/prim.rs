//! Standard library definitions

use Prim;

pub fn words() -> Prim {
    Prim::new("('A -> 'A)", defs::words)
}

pub fn dup() -> Prim {
    Prim::new("('A 'b -> 'A 'b 'b)", defs::dup)
}

pub fn pop() -> Prim {
    Prim::new("('A 'b -> 'A)", defs::pop)
}

pub fn swap() -> Prim {
    Prim::new("('A 'b 'c -> 'A 'c 'b)", defs::swap)
}

pub fn apply() -> Prim {
    Prim::new("('A ('A -> 'B) -> 'B)", defs::apply)
}

pub fn quote() -> Prim {
    Prim::new("('A 'b -> 'A ('C -> 'C 'b))", defs::quote)
}

pub fn compose() -> Prim {
    Prim::new("('A ('B -> 'C) ('C -> 'D) -> 'A ('B -> 'D))", defs::compose)
}

pub fn if_() -> Prim {
    Prim::new("('A bool ('A -> 'B) ('A -> 'B) -> 'B)", defs::if_)
}

pub fn eq() -> Prim {
    Prim::new("('A bool bool -> 'A bool)", defs::eq)
}

pub fn and() -> Prim {
    Prim::new("('A bool bool -> 'A bool)", defs::and)
}

pub fn or() -> Prim {
    Prim::new("('A bool bool -> 'A bool)", defs::or)
}

pub fn not() -> Prim {
    Prim::new("('A bool -> 'A bool)", defs::not)
}

pub fn add() -> Prim {
    Prim::new("('A num num -> 'A num)", defs::add)
}

pub fn sub() -> Prim {
    Prim::new("('A num num -> 'A num)", defs::sub)
}

pub fn mul() -> Prim {
    Prim::new("('A num num -> 'A num)", defs::mul)
}

pub fn div() -> Prim {
    Prim::new("('A num num -> 'A num)", defs::div)
}

pub fn rem() -> Prim {
    Prim::new("('A num num -> 'A num)", defs::rem)
}

mod defs {
    use std::ops::*;

    use {EvalResult, StackTerm, Term, Value, Words};

    pub fn words(stack_term: StackTerm, words: &Words) -> EvalResult {
        for name in words.defs.keys() {
            println!("{}", name);
        }
        Ok(stack_term)
    }

    pub fn dup(stack_term: StackTerm, _: &Words) -> EvalResult {
        let term = try!(stack_term.peek()).clone();
        Ok(stack_term.push(term))
    }

    pub fn pop(stack_term: StackTerm, _: &Words) -> EvalResult {
        let (stack_term, _) = try!(stack_term.pop());
        Ok(stack_term)
    }

    pub fn swap(stack_term: StackTerm, _: &Words) -> EvalResult {
        let (stack_term, term_a) = try!(stack_term.pop());
        let (stack_term, term_b) = try!(stack_term.pop());

        Ok(stack_term.push(term_a).push(term_b))
    }

    pub fn apply(stack_term: StackTerm, words: &Words) -> EvalResult {
        let (stack_term, quote) = try!(stack_term.pop_quote());

        stack_term.eval_stack(words, quote)
    }

    pub fn quote(stack_term: StackTerm, _: &Words) -> EvalResult {
        let (stack_term, term) = try!(stack_term.pop());
        let quoted = Term::Quote(StackTerm::new(vec![term]));

        Ok(stack_term.push(quoted))
    }

    pub fn compose(stack_term: StackTerm, _: &Words) -> EvalResult {
        let (stack_term, stack_a) = try!(stack_term.pop_quote());
        let (stack_term, mut stack_b) = try!(stack_term.pop_quote());

        stack_b.terms.extend(stack_a.terms);

        Ok(stack_term.push(Term::Quote(stack_b)))
    }

    pub fn if_(stack_term: StackTerm, words: &Words) -> EvalResult {
        let (stack_term, alt) = try!(stack_term.pop_quote());
        let (stack_term, conseq) = try!(stack_term.pop_quote());
        let (stack_term, pred) = try!(stack_term.pop_bool());

        stack_term.eval_stack(words, if pred { conseq } else { alt })
    }

    pub fn eq(stack_term: StackTerm, _: &Words) -> EvalResult {
        let (stack_term, y) = try!(stack_term.pop_number());
        let (stack_term, x) = try!(stack_term.pop_number());

        Ok(stack_term.push(Term::Push(Value::Bool(x == y))))
    }

    pub fn and(stack_term: StackTerm, _: &Words) -> EvalResult {
        let (stack_term, y) = try!(stack_term.pop_bool());
        let (stack_term, x) = try!(stack_term.pop_bool());

        Ok(stack_term.push(Term::Push(Value::Bool(x && y))))
    }

    pub fn or(stack_term: StackTerm, _: &Words) -> EvalResult {
        let (stack_term, y) = try!(stack_term.pop_bool());
        let (stack_term, x) = try!(stack_term.pop_bool());

        Ok(stack_term.push(Term::Push(Value::Bool(x || y))))
    }

    pub fn not(stack_term: StackTerm, _: &Words) -> EvalResult {
        let (stack_term, x) = try!(stack_term.pop_bool());

        Ok(stack_term.push(Term::Push(Value::Bool(!x))))
    }

    fn apply_binop<F: Fn(i32, i32) -> i32>(stack_term: StackTerm, f: F) -> EvalResult {
        let (stack_term, y) = try!(stack_term.pop_number());
        let (stack_term, x) = try!(stack_term.pop_number());

        Ok(stack_term.push(Term::Push(Value::Number(f(x, y)))))
    }

    pub fn add(stack_term: StackTerm, _: &Words) -> EvalResult {
        apply_binop(stack_term, i32::add)
    }

    pub fn sub(stack_term: StackTerm, _: &Words) -> EvalResult {
        apply_binop(stack_term, i32::sub)
    }

    pub fn mul(stack_term: StackTerm, _: &Words) -> EvalResult {
        apply_binop(stack_term, i32::mul)
    }

    pub fn div(stack_term: StackTerm, _: &Words) -> EvalResult {
        apply_binop(stack_term, i32::div)
    }

    pub fn rem(stack_term: StackTerm, _: &Words) -> EvalResult {
        apply_binop(stack_term, i32::rem)
    }
}
