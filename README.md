# Introduction

Haskell implementation of the lambda calculi covered in the [Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/) book. Work in progress. 

A web REPL / tutorial based on this is available [here](https://vincent-prz.github.io/lambda-calculus-repl/). In case you wonder, the code for this can be found in the [browser branch](https://github.com/vincent-prz/tapl/tree/browser).

As of today, 2 variants of lambda calculus are covered: the regular untyped lambda calculus, and the simply typed lambda calculus.

# How to run it

The following command will fire an untyped lambda calculus REPL.

```
stack run untyped
```

The following command will fire a simply typed lambda calculus REPL.

```
stack run typed
```

# Examples: untyped lambda calculus

```ml
> \x.x
\x.x
> \t.\f.t
\t.\f.t
> (\t.\f.\t) (\x.x) (\x.x x)
\x.x
> id = \x.x
()
> id \t.\f.t
\t.\f.t
```

# Examples: simply typed lambda calculus

Note: to avoid grammar ambiguity problems, the application is done via the `$` operator.

```ml
> \x : Bool.true
\x:Bool.true : Bool->Bool
> \x : Nat.succ x
\x:Nat.succ x : Nat->Nat
> (\x : Bool. if x then 0 else succ 0) $ false
succ 0 : Nat
> (\x : Bool. if x then 0 else succ 0) $ 0
ArgMisMatch {expected = Bool, got = Nat}
```

# Options (Untyped only)

2 evaluations strategies are supported: call by value and full beta reduction. The main difference is that the latter will reduce the expressions fully, while call by value will not reduce anything inside abstractions.

## Evaluation strategy

```ml
> :beta
Switching to full beta evaluation mode.
> \x.(\x.x) x
\x.x
> :cbv
Switching to call by value evaluation mode.
> \x.(\x.x) x
\x.(\x.x) x
```

## Verbose mode

It is possible to display all the reduction steps:
```ml
> :verbose on
Enabling verbose mode.
> (\x.\y.y) (\s.\z.z) (\x.(\x.x) x)
(\x.\y.y) \s.\z.z \x.(\x.x) x
(\y.y) \x.x
\x.x
> :verbose off
Disabling verbose mode.
```

# Status

## Language features
 - [x] Untyped lambda calculus
 - [x] Simply typed lambda calculus
 - [ ] Simple extensions (Let bindings, records, sum types ...)
 - [ ] Subtyping
 - [ ] Recursive types
 - [ ] Polymorphism
 - [ ] Higher-Order systems

## Project features
 - [X] A CLI REPL
 - [X] A web REPL
 - [X] 2 evaluation strategies supported: Call by value and full beta reduction
 - [X] possibility to print all the reduction steps
 - [X] Variable assignment feature
 - [ ] Simply typed lambda calculus
 
 ...
