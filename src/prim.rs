//! Primitive operations

use std::ops::*;
use super::*;

// dup : (A b -> A b b)
pub fn dup(ev: &mut Evaluator) -> EvalResult<()> {
    let term = try!(ev.stack.peek()).clone();
    ev.stack.push(term);
    Ok(())
}

// pop : (A b -> A)
pub fn pop(ev: &mut Evaluator) -> EvalResult<()> {
    ev.stack.pop().map(|_| ())
}

// swap : (A b c -> A c b)
pub fn swap(ev: &mut Evaluator) -> EvalResult<()> {
    let term_a = try!(ev.stack.pop());
    let term_b = try!(ev.stack.pop());

    ev.stack.push(term_a);
    ev.stack.push(term_b);

    Ok(())
}

// apply : (A (A -> B) -> B)
pub fn apply(ev: &mut Evaluator) -> EvalResult<()> {
    let quote = try!(ev.stack.pop_quote());

    ev.eval_quote(quote)
}

// quote : (A b -> A (C -> C b))
pub fn quote(ev: &mut Evaluator) -> EvalResult<()> {
    let term = try!(ev.stack.pop());
    let quoted = Term::Quote(Stack::new(vec![term]));

    ev.stack.push(quoted);

    Ok(())
}

// compose : (A (B -> C) (C -> D) -> A (B -> D)))
pub fn compose(ev: &mut Evaluator) -> EvalResult<()> {
    let stack_a = try!(ev.stack.pop_quote());
    let mut stack_b = try!(ev.stack.pop_quote());

    stack_b.terms.extend(stack_a.terms);
    ev.stack.push(Term::Quote(stack_b));

    Ok(())
}

// words : (A ~> A)
pub fn words(ev: &mut Evaluator) -> EvalResult<()> {
    for name in ev.words.defs.keys() {
        println!("{}", name);
    }

    Ok(())
}

fn apply_binop<F: Fn(i32, i32) -> i32>(stack: &mut Stack, f: F) -> EvalResult<()> {
    let x = try!(stack.pop_number());
    let y = try!(stack.pop_number());

    stack.push(Term::Number(f(x, y)));

    Ok(())
}

// add : (A num num -> A num)
pub fn add(ev: &mut Evaluator) -> EvalResult<()> {
    apply_binop(&mut ev.stack, i32::add)
}

// sub : (A num num -> A num)
pub fn sub(ev: &mut Evaluator) -> EvalResult<()> {
    apply_binop(&mut ev.stack, i32::sub)
}

// mul : (A num num -> A num)
pub fn mul(ev: &mut Evaluator) -> EvalResult<()> {
    apply_binop(&mut ev.stack, i32::mul)
}

// div : (A num num -> A num)
pub fn div(ev: &mut Evaluator) -> EvalResult<()> {
    apply_binop(&mut ev.stack, i32::div)
}

// rem : (A num num -> A num)
pub fn rem(ev: &mut Evaluator) -> EvalResult<()> {
    apply_binop(&mut ev.stack, i32::rem)
}
