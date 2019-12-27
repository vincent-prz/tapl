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
# Browser version

I'm also making a browser version of my untyped lambda calculus repl, it can be found [here](https://vincent-prz.github.io/). It corresponds to the `browser` branch of this project.
