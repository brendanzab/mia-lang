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

    use {EvalResult, Stack, Term, Value, Words};

    pub fn words(stack: Stack, words: &Words) -> EvalResult {
        for name in words.defs.keys() {
            println!("{}", name);
        }
        Ok(stack)
    }

    pub fn dup(stack: Stack, _: &Words) -> EvalResult {
        let term = try!(stack.peek()).clone();
        Ok(stack.push(term))
    }

    pub fn pop(stack: Stack, _: &Words) -> EvalResult {
        let (stack, _) = try!(stack.pop());
        Ok(stack)
    }

    pub fn swap(stack: Stack, _: &Words) -> EvalResult {
        let (stack, term_a) = try!(stack.pop());
        let (stack, term_b) = try!(stack.pop());

        Ok(stack.push(term_a).push(term_b))
    }

    pub fn apply(stack: Stack, words: &Words) -> EvalResult {
        let (stack, quote) = try!(stack.pop_quote());

        stack.eval_stack(words, quote)
    }

    pub fn quote(stack: Stack, _: &Words) -> EvalResult {
        let (stack, term) = try!(stack.pop());
        let quoted = Term::Quote(Stack::new(vec![term]));

        Ok(stack.push(quoted))
    }

    pub fn compose(stack: Stack, _: &Words) -> EvalResult {
        let (stack, stack_a) = try!(stack.pop_quote());
        let (stack, mut stack_b) = try!(stack.pop_quote());

        stack_b.terms.extend(stack_a.terms);

        Ok(stack.push(Term::Quote(stack_b)))
    }

    pub fn if_(stack: Stack, words: &Words) -> EvalResult {
        let (stack, alt) = try!(stack.pop_quote());
        let (stack, conseq) = try!(stack.pop_quote());
        let (stack, pred) = try!(stack.pop_bool());

        stack.eval_stack(words, if pred { conseq } else { alt })
    }

    pub fn eq(stack: Stack, _: &Words) -> EvalResult {
        let (stack, y) = try!(stack.pop_number());
        let (stack, x) = try!(stack.pop_number());

        Ok(stack.push(Term::Push(Value::Bool(x == y))))
    }

    pub fn and(stack: Stack, _: &Words) -> EvalResult {
        let (stack, y) = try!(stack.pop_bool());
        let (stack, x) = try!(stack.pop_bool());

        Ok(stack.push(Term::Push(Value::Bool(x && y))))
    }

    pub fn or(stack: Stack, _: &Words) -> EvalResult {
        let (stack, y) = try!(stack.pop_bool());
        let (stack, x) = try!(stack.pop_bool());

        Ok(stack.push(Term::Push(Value::Bool(x || y))))
    }

    pub fn not(stack: Stack, _: &Words) -> EvalResult {
        let (stack, x) = try!(stack.pop_bool());

        Ok(stack.push(Term::Push(Value::Bool(!x))))
    }

    fn apply_binop<F: Fn(i32, i32) -> i32>(stack: Stack, f: F) -> EvalResult {
        let (stack, y) = try!(stack.pop_number());
        let (stack, x) = try!(stack.pop_number());

        Ok(stack.push(Term::Push(Value::Number(f(x, y)))))
    }

    pub fn add(stack: Stack, _: &Words) -> EvalResult {
        apply_binop(stack, i32::add)
    }

    pub fn sub(stack: Stack, _: &Words) -> EvalResult {
        apply_binop(stack, i32::sub)
    }

    pub fn mul(stack: Stack, _: &Words) -> EvalResult {
        apply_binop(stack, i32::mul)
    }

    pub fn div(stack: Stack, _: &Words) -> EvalResult {
        apply_binop(stack, i32::div)
    }

    pub fn rem(stack: Stack, _: &Words) -> EvalResult {
        apply_binop(stack, i32::rem)
    }
}
