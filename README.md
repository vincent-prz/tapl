# Introduction

Haskell implementation of the lambda calculi covered in the [Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/) book. Work in progress. 

A web REPL / tutorial based on this is available [here](https://vincent-prz.github.io/lambda-calculus-repl/). In case you wonder, the code for this can be found in the [browser branch](https://github.com/vincent-prz/tapl/tree/browser).

# How to run it

The following command will fire a lambda calculus REPL.

```
stack run
```

# Examples

```
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
