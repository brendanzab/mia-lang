//! Primitive operations

use std::ops::*;
use super::*;

// words : (A ~> A)
pub fn words(_: &mut Stack, words: &Words) -> EvalResult<()> {
    for name in words.defs.keys() {
        println!("{}", name);
    }

    Ok(())
}

// dup : (A b -> A b b)
pub fn dup(stack: &mut Stack, _: &Words) -> EvalResult<()> {
    let term = try!(stack.peek()).clone();
    stack.push(term);
    Ok(())
}

// pop : (A b -> A)
pub fn pop(stack: &mut Stack, _: &Words) -> EvalResult<()> {
    stack.pop().map(|_| ())
}

// swap : (A b c -> A c b)
pub fn swap(stack: &mut Stack, _: &Words) -> EvalResult<()> {
    let term_a = try!(stack.pop());
    let term_b = try!(stack.pop());

    stack.push(term_a);
    stack.push(term_b);

    Ok(())
}

// apply : (A (A -> B) -> B)
pub fn apply(stack: &mut Stack, words: &Words) -> EvalResult<()> {
    let quote = try!(stack.pop_quote());

    stack.eval_stack(words, quote)
}

// quote : (A b -> A (C -> C b))
pub fn quote(stack: &mut Stack, _: &Words) -> EvalResult<()> {
    let term = try!(stack.pop());
    let quoted = Term::Quote(Stack::new(vec![term]));

    stack.push(quoted);

    Ok(())
}

// compose : (A (B -> C) (C -> D) -> A (B -> D)))
pub fn compose(stack: &mut Stack, _: &Words) -> EvalResult<()> {
    let stack_a = try!(stack.pop_quote());
    let mut stack_b = try!(stack.pop_quote());

    stack_b.terms.extend(stack_a.terms);
    stack.push(Term::Quote(stack_b));

    Ok(())
}

// if : (A bool (A -> B) (A -> B) -> B)
pub fn if_(stack: &mut Stack, words: &Words) -> EvalResult<()> {
    let alt = try!(stack.pop_quote());
    let conseq = try!(stack.pop_quote());
    let pred = try!(stack.pop_bool());

    stack.eval_stack(words, if pred { conseq } else { alt })
}

// eq : (A bool bool -> A bool)
pub fn eq(stack: &mut Stack, _: &Words) -> EvalResult<()> {
    let y = try!(stack.pop_number());
    let x = try!(stack.pop_number());

    stack.push(Term::Push(Value::Bool(x == y)));

    Ok(())
}

// and : (A bool bool -> A bool)
pub fn and(stack: &mut Stack, _: &Words) -> EvalResult<()> {
    let y = try!(stack.pop_bool());
    let x = try!(stack.pop_bool());

    stack.push(Term::Push(Value::Bool(x && y)));

    Ok(())
}

// or : (A bool bool -> A bool)
pub fn or(stack: &mut Stack, _: &Words) -> EvalResult<()> {
    let y = try!(stack.pop_bool());
    let x = try!(stack.pop_bool());

    stack.push(Term::Push(Value::Bool(x || y)));

    Ok(())
}

// not : (A bool -> A bool)
pub fn not(stack: &mut Stack, _: &Words) -> EvalResult<()> {
    let x = try!(stack.pop_bool());

    stack.push(Term::Push(Value::Bool(!x)));

    Ok(())
}

fn apply_binop<F: Fn(i32, i32) -> i32>(stack: &mut Stack, f: F) -> EvalResult<()> {
    let y = try!(stack.pop_number());
    let x = try!(stack.pop_number());

    stack.push(Term::Push(Value::Number(f(x, y))));

    Ok(())
}

// add : (A num num -> A num)
pub fn add(stack: &mut Stack, _: &Words) -> EvalResult<()> {
    apply_binop(stack, i32::add)
}

// sub : (A num num -> A num)
pub fn sub(stack: &mut Stack, _: &Words) -> EvalResult<()> {
    apply_binop(stack, i32::sub)
}

// mul : (A num num -> A num)
pub fn mul(stack: &mut Stack, _: &Words) -> EvalResult<()> {
    apply_binop(stack, i32::mul)
}

// div : (A num num -> A num)
pub fn div(stack: &mut Stack, _: &Words) -> EvalResult<()> {
    apply_binop(stack, i32::div)
}

// rem : (A num num -> A num)
pub fn rem(stack: &mut Stack, _: &Words) -> EvalResult<()> {
    apply_binop(stack, i32::rem)
}
