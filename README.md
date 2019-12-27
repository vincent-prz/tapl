# Introduction

Haskell implementation of the lambda calculi covered in the [Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/) book. Work in progress. 

A web REPL / tutorial based on this is available [here](https://vincent-prz.github.io/lambda-calculus-repl/). In case you wonder, the code for this can be found in the [browser branch](https://github.com/vincent-prz/tapl/tree/browser).

# How to run it

The following command will fire a lambda calculus REPL.

```
stack run
```

# Examples

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

# Options

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

 - [x] Untyped lambda calculus
 - [X] A CLI REPL
 - [X] A web REPL
 - [X] 2 evaluation strategies supported: Call by value and full beta reduction
 - [X] possibility to print all the reduction steps
 - [X] Variable assignment feature
 - [ ] Simply typed lambda calculus
 
 ...
