# ThomsonSolve
A Haskell Based Generalised Thomson Problem Solver (High Lower Bound)


## What does this do?
Currently, this program iteratively spaces 'k' points inside a unit n-ball optimally using an approximated Riesz s-energy method.

## What will this project do in the future?
This program hopes to use the cartesian product of 2-balls to gain a space advantage and be able to place more points inside the volume.
A small modification that I will make soon is to place the points on the (n-1)-sphere.

## How do I run this or view documentation?
To view documentation run `pdflatex thomson.lhs`

To compile for the program run `ghc -o thomson thomson.lhs`

Currently there is no `main` so you may want to just load into GHCi `:l thomson`

## FAQs
### Why was this even made?
This project was made to explore space-optimal structures for high dimensional communications - if you want more details you can have a look at my blog when the project is live.

### Why did you write this in Haskell?
I was in the Haskell folder on my computer and I was too lazy to move to the python folder to start this project.
I was also rusty in my Haskell and I needed the practise.

### Why is your indenting so bad?
I use a [Haskell Concealer](https://github.com/enomsg/vim-haskellConcealPlus) in vim! 
