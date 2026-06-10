# ThomsonSolve
A Haskell Based Generalised Thomson Problem Solver (High Lower Bound)


## What does this do?
Currently, this program iteratively spaces 'k' points inside a unit n-ball optimally using an approximated Riesz s-energy method.

## What will this project do in the future?
This program hopes to use the cartesian product of 2-balls to gain a space advantage and be able to place more points inside the volume.
A small modification that I will make soon is to place the points on the (n-1)-sphere.

## How do I run this or view documentation?
The program depends on `random`, `split`, `Chart` and `Chart-diagrams`. A
minimal `thomson.cabal` is included, so the easiest way to build and run is:

```
cabal run thomson
```

This runs the example simulation (16 points in the 2-ball), prints the achieved
minimum pairwise distance, and writes the plot to `thomson.svg` — the plotting is
done entirely in Haskell, so no external tooling is needed.

To experiment interactively, load it into GHCi with `cabal repl` and call e.g.
`debugRun 2 16 0.001 4 10000` for a reproducible (fixed-seed) run.

To view the documentation/paper run `pdflatex thomson.lhs`.

## FAQs
### Why was this even made?
This project was made to explore space-optimal structures for high dimensional communications - if you want more details you can have a look at my blog when the project is live.

### Why did you write this in Haskell?
I was in the Haskell folder on my computer and I was too lazy to move to the python folder to start this project.
I was also rusty in my Haskell and I needed the practise.

### Why is your indenting so bad?
I use a [Haskell Concealer](https://github.com/enomsg/vim-haskellConcealPlus) in vim! 
