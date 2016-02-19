# Specification

This is an attempt to formalise the semantics of the language. I read the
[Cat Language's technical report](https://web.archive.org/web/20131116070738/http://www.cat-language.com/Cat-TR-2008-001.pdf),
but after attempting to implement it I found it lacking in key details.
Perusing the reference implementation was also a challenge, due to the
difficulty in implementing languages in C# in a clear, concise way.

Do note that I am self-taught, and that there are probably egregious holes in
my reasoning.

## Formal semantics

NOTE: This section is still incomplete!

#### Syntax

Unlike the Cat TR, I have decided to include the empty stack, `ε`, and its
type `E`, in order to enable the pattern matching on quotations.

```text

a ::=                       atomic values
        bv                  boolean value
        nv                  numeric value
        ...

v ::=                       values:
        a                   atomic value
        [ s ]               quotation

t ::=                       terms:
        x                   variable
        v                   pushed value

s ::=                       stack terms:
        ε                   empty stack
        s t                 stack with a term on top

```

```text

A ::=
       bool                 type of booleans
       num                  type of numbers
       ...

T ::=                       types:
        A                   atomic type
        (S -> S)            type of stack functions

S ::=                       stack types:
        E                   empty stack type
        S T                 stack type with a type on top

Γ ::=                       contexts:
        ∅                   empty context
        Γ, x : T            variable binding

```

#### Evaluation `[ s → s′ ]`

TODO

#### Typing `[ Γ ⊢ t : T ]`

```text

  x : T ∈ Γ
------------- (T-Call)
  Γ ⊢ x : T


       Γ ⊢ a : A
----------------------- (T-Atom)
  Γ ⊢ a : (S -> S A)


        Γ ⊢ s : E (S₁ -> S₂)
------------------------------------- (T-Quote)
  Γ ⊢ [ s ] : (S₀ -> S₀ (S₁ -> S₂))

```

#### Stack Typing `[ Γ ⊢ s : S ]`

```text

ε : E     (S-Empty)


  Γ ⊢ s : S₀ (S₁ -> S₂)     Γ ⊢ t : (S₂ -> S₄)
------------------------------------------------ (S-Compose)
            Γ ⊢ s t : S₀ (S₁ -> S₄)

```

#### Constraint generation `[ Γ ⊢ t : T | C ]`

TODO

## References

TODO
