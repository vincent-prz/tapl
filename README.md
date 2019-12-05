# Introduction

Haskell implementation of the lambda calculi covered in the [Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/) book. Work in progress. 

# How to run it

The following command will spawn a warp server on port 3003, with a lambda calculus interpreter. NOTE: it doesn't work in Firefox, apparently because of [a JSaddle bug](https://github.com/ghcjs/jsaddle/issues/64).

```
nix-shell -A shells.ghc --run 'cabal v2-run tapl-exe'
```

# How to build it

GHC:

```
> nix-build -A ghc.tapl
```

GHCJS:

```
> nix-build -A ghcjs.tapl
```

# Examples

```
> \x.x
\x.x
> \t.\f.t
\t.\f.t
> (\t.\f.\t) (\x.x) (\x.x x)
\x.x
```
# Browser version

I'm also making a browser version of my untyped lambda calculus repl, it can be found [here](https://vincent-prz.github.io/). It corresponds to the `browser` branch of this project.
