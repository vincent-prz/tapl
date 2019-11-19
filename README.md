# Introduction

Haskell implementation of the lambda calculi covered in the [Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/) book. Work in progress.

# How to run it

The following command will fire a lambda calculus interpreter.

```
stack run
```

# Examples

```
> \x.x
\a.a
> \t.\f.t
\a.\b.a
> \x.\y. x y
\a.\b.(a b)
```
