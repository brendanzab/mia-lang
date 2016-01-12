//! Primitive operations

use std::ops::*;

use {EvalResult, Stack, Term, Value, Words};

// words : (A ~> A)
pub fn words(stack: Stack, words: &Words) -> EvalResult {
    for name in words.defs.keys() {
        println!("{}", name);
    }
    Ok(stack)
}

// dup : (A b -> A b b)
pub fn dup(stack: Stack, _: &Words) -> EvalResult {
    let term = try!(stack.peek()).clone();
    Ok(stack.push(term))
}

// pop : (A b -> A)
pub fn pop(stack: Stack, _: &Words) -> EvalResult {
    let (stack, _) = try!(stack.pop());
    Ok(stack)
}

// swap : (A b c -> A c b)
pub fn swap(stack: Stack, _: &Words) -> EvalResult {
    let (stack, term_a) = try!(stack.pop());
    let (stack, term_b) = try!(stack.pop());

    Ok(stack.push(term_a).push(term_b))
}

// apply : (A (A -> B) -> B)
pub fn apply(stack: Stack, words: &Words) -> EvalResult {
    let (stack, quote) = try!(stack.pop_quote());

    stack.eval_stack(words, quote)
}

// quote : (A b -> A (C -> C b))
pub fn quote(stack: Stack, _: &Words) -> EvalResult {
    let (stack, term) = try!(stack.pop());
    let quoted = Term::Quote(Stack::new(vec![term]));

    Ok(stack.push(quoted))
}

// compose : (A (B -> C) (C -> D) -> A (B -> D)))
pub fn compose(stack: Stack, _: &Words) -> EvalResult {
    let (stack, stack_a) = try!(stack.pop_quote());
    let (stack, mut stack_b) = try!(stack.pop_quote());

    stack_b.terms.extend(stack_a.terms);

    Ok(stack.push(Term::Quote(stack_b)))
}

// if : (A bool (A -> B) (A -> B) -> B)
pub fn if_(stack: Stack, words: &Words) -> EvalResult {
    let (stack, alt) = try!(stack.pop_quote());
    let (stack, conseq) = try!(stack.pop_quote());
    let (stack, pred) = try!(stack.pop_bool());

    stack.eval_stack(words, if pred { conseq } else { alt })
}

// eq : (A bool bool -> A bool)
pub fn eq(stack: Stack, _: &Words) -> EvalResult {
    let (stack, y) = try!(stack.pop_number());
    let (stack, x) = try!(stack.pop_number());

    Ok(stack.push(Term::Push(Value::Bool(x == y))))
}

// and : (A bool bool -> A bool)
pub fn and(stack: Stack, _: &Words) -> EvalResult {
    let (stack, y) = try!(stack.pop_bool());
    let (stack, x) = try!(stack.pop_bool());

    Ok(stack.push(Term::Push(Value::Bool(x && y))))
}

// or : (A bool bool -> A bool)
pub fn or(stack: Stack, _: &Words) -> EvalResult {
    let (stack, y) = try!(stack.pop_bool());
    let (stack, x) = try!(stack.pop_bool());

    Ok(stack.push(Term::Push(Value::Bool(x || y))))
}

// not : (A bool -> A bool)
pub fn not(stack: Stack, _: &Words) -> EvalResult {
    let (stack, x) = try!(stack.pop_bool());

    Ok(stack.push(Term::Push(Value::Bool(!x))))
}

fn apply_binop<F: Fn(i32, i32) -> i32>(stack: Stack, f: F) -> EvalResult {
    let (stack, y) = try!(stack.pop_number());
    let (stack, x) = try!(stack.pop_number());

    Ok(stack.push(Term::Push(Value::Number(f(x, y)))))
}

// add : (A num num -> A num)
pub fn add(stack: Stack, _: &Words) -> EvalResult {
    apply_binop(stack, i32::add)
}

// sub : (A num num -> A num)
pub fn sub(stack: Stack, _: &Words) -> EvalResult {
    apply_binop(stack, i32::sub)
}

// mul : (A num num -> A num)
pub fn mul(stack: Stack, _: &Words) -> EvalResult {
    apply_binop(stack, i32::mul)
}

// div : (A num num -> A num)
pub fn div(stack: Stack, _: &Words) -> EvalResult {
    apply_binop(stack, i32::div)
}

// rem : (A num num -> A num)
pub fn rem(stack: Stack, _: &Words) -> EvalResult {
    apply_binop(stack, i32::rem)
}
