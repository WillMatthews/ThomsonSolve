\documentclass[11pt,oneside,a4paper]{article}

\usepackage{hyperref}
\usepackage{mathtools}
\usepackage{xcolor}
\usepackage{amsfonts}
\usepackage{listings}
\lstloadlanguages{Haskell}
\lstnewenvironment{code}
    {\lstset{}%
      \csname lst@SetFirstLabel\endcsname}
    {\csname lst@SaveFirstLabel\endcsname}
    \lstset{
      basicstyle=\small\ttfamily,
      flexiblecolumns=false,
      breaklines=true,
      frame=single,
      postbreak=\mbox{\textcolor{red}{$\hookrightarrow$}\space},
      basewidth={0.5em,0.45em},
      literate={+}{{$+$}}1 {/}{{$/$}}1 {*}{{$*$}}1 {=}{{$=$}}1
               {>}{{$>$}}1 {<}{{$<$}}1 {\\}{{$\lambda$}}1
               {\\\\}{{\char`\\\char`\\}}1
               {->}{{$\rightarrow$}}2 {>=}{{$\geq$}}2 {<-}{{$\leftarrow$}}2
               {<=}{{$\leq$}}1 {=>}{{$\Rightarrow$}}1
               {\ .}{{$\circ$}}2 {\ .\ }{{$\circ$}}2
               {>>}{{>>}}2 {>>=}{{>>=}}2
               {|}{{$\mid$}}1
    }

\DeclarePairedDelimiter{\abs}{\lvert}{\rvert}
\DeclarePairedDelimiter{\norm}{\lVert}{\rVert}%

\title{An Iterative Riesz s-energy Based Solution to Achieving High Maximum Minimum Distance Between Points in the $n$-Ball.}
\date{April 2019}
\author{William Matthews\\ Masters Undergraduate of Engineering Science, University of Oxford}



\begin{document}
\maketitle

\section{Disclaimer}
This was all written by an engineering undergraduate. Exercise reasonable caution and do not believe anything you see here written.

\section{Problem Definition and Use Case}
Research has revealed high dimensional `one hot vector' codebooks for communications have had little work in their optimisation.
As noise in a communications system corrupts symbols, the most robust BER performance is obtained by \emph{maximising minimum $L_2$ distance between constellation points}.
One sense of defining an optimal codebook $\mathcal{C}_{\text{optimal}}$ is
\begin{equation}
    \mathcal{C}_{\text{optimal}} = \arg \max_{\mathcal{C} \subset \mathfrak{S}} \min_{X_{i}, X_{j} \in \mathcal{C}, i \neq j} \norm {X_i - X_j}_2
\end{equation}
where $\mathfrak{S}$ is the space avaliable to the transmission scheme.

Due to the work on my fourth year project, an algorithm was developed for the case of the ($n-1$)-sphere and $n$-ball.
This developed algorithm is concerned with the $n$-ball as the volume offers more space for point placement and resembles a \emph{symbol power limit}.
Solutions were found to exist for point spacing on the ($n-1$)-sphere, so this work was primarily concerned in spacing inside the $n$-ball.
\emph{Future work} will be in modifying this existing code to work with 2-ball cartesian products (independant channel power limit).

\emph{No work has been done on Gray Coding the developed constellation, and is a problem left to be solved.}

\section{Algorithm}
The code for this project is written in Haskell (a pure functional language) to ensure safe code.
While Haskell is a high level language the program still performs adequately highly due to Haskell being a compiled language.

This section details the code in its entirety and highlights any important steps.
A notation detail is that the red hooked arrow {\textcolor{red}{$\hookrightarrow$} denotes text on the same line but wrapped.

\subsection{Imports}
Only \texttt{System.Random}\footnote{Installable by doing \texttt{cabal install random}} and \texttt{Data.List.Split}\footnote{Installable by doing \texttt{cabal install split}} are needed for the simulation itself, and \texttt{Control.Monad} supplies \texttt{replicateM} for the initialisation code.
Plotting is handled entirely in Haskell via the \texttt{Chart} library with its diagrams backend\footnote{Installable by doing \texttt{cabal install Chart Chart-diagrams}}, which renders an SVG without any external tooling.

\begin{code}
module Main where

import System.Random
import Data.List (transpose)
import Data.List.Split (chunksOf)
import Control.Monad (replicateM)

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams (toFile, fo_size)
\end{code}

\subsection{Data Types}
The simulation is described by two record types.
A \texttt{Particle} bundles a constellation point's position and velocity, and a \texttt{Sim} holds the whole simulation state: the time step \texttt{dt}, the Riesz exponent \texttt{sExp}, and the list of particles.
Using records (rather than nested tuples) means every field is named, which keeps the rest of the code readable and lets us update the state with record-update syntax.
A \texttt{Vec} is just an alias for a list of \texttt{Double}s, treated as a vector in $\mathbb{R}^d$.

\begin{code}
type Vec = [Double]

data Particle = Particle
    { pos :: Vec   -- position in the n-ball
    , vel :: Vec   -- velocity
    } deriving (Show)

data Sim = Sim
    { dt        :: Double      -- Euler time step
    , sExp      :: Int         -- Riesz s-energy exponent
    , particles :: [Particle]
    } deriving (Show)
\end{code}

\subsection{Vector Operations}
Some general vector operations are defined since the functionality doesn't natively exist in Haskell: element-wise addition and subtraction, scaling by a constant, the $L_2$ norm, and normalisation to a unit vector.
The function \texttt{clampNorm} projects a vector back onto the sphere of a given radius whenever it grows longer than that radius; this is what keeps positions inside the $n$-ball and caps velocities.

\begin{code}
vadd :: Vec -> Vec -> Vec
vadd = zipWith (+)

vsub :: Vec -> Vec -> Vec
vsub = zipWith (-)

scaleVec :: Double -> Vec -> Vec
scaleVec c = map (c *)

-- L2 norm
l2 :: Vec -> Double
l2 v = sqrt (sum (map (^ (2 :: Int)) v))

normalise :: Vec -> Vec
normalise v = scaleVec (1 / l2 v) v

-- if |v| exceeds the cap, rescale it onto the sphere of radius `cap`
clampNorm :: Double -> Vec -> Vec
clampNorm cap v
    | l2 v > cap = scaleVec cap (normalise v)
    | otherwise  = v
\end{code}

\subsection{Initialisation Code}
A particle is created with a uniformly random position in $[-0.5, 0.5]^d$ and zero velocity by \texttt{randomParticle}, which lives in the \texttt{IO} monad because it draws randomness.
\texttt{initSim} builds a full \texttt{Sim} of \texttt{k} such particles in \texttt{d} dimensions, with \texttt{dt} and the Riesz exponent supplied by the caller.
Care is taken to keep the \texttt{IO} on the outside, so the rest of the pipeline stays pure.

\begin{code}
-- a single particle: random position, zero velocity
randomParticle :: Int -> IO Particle
randomParticle d = do
    p <- replicateM d (randomRIO (-0.5, 0.5))
    return (Particle p (replicate d 0))

-- initial state: k particles in d dimensions
initSim :: Int -> Int -> Double -> Int -> IO Sim
initSim d k dt' s = do
    ps <- replicateM k (randomParticle d)
    return (Sim dt' s ps)
\end{code}

\subsection{The Transition Function}
The transition function \texttt{step} maps a \texttt{Sim} to a \texttt{Sim}, so it can be composed (iterated) any number of times, $a_n = \texttt{step}(\texttt{step}(\cdots(a_0)\cdots))$.
Each call advances the simulation by one Euler time step: it computes the inter-point `forces', turns them into accelerations, and updates every particle's velocity and then position.

\subsubsection{Force Function - Notes on Riesz s-Energy}
The `force' on a point $p$ from another point $q$ under the Riesz s-energy method is
\begin{equation}
    F_{p,q} = \frac{p - q}{\norm{p-q}_2^{s+1}}
\end{equation}
for some $s \in \mathbb{N}$, which is repulsive (it pushes $p$ away from $q$).
For the equispacing case, $s$ is driven to infinity to make the closest point dominate.
A way of visualising the $s \rightarrow \infty$ case is each point having a hard sphere around it --- and once points are equispaced the spheres (of identical radius) touch their neighbours.

Taken literally the expression above is numerically unstable for large $s$: when $\norm{p-q}_2 < 1$ the denominator $\norm{p-q}_2^{s+1}$ underflows to zero (an infinite force), and when $\norm{p-q}_2 > 1$ it overflows.
To avoid this we rescale every distance by the \emph{current minimum pairwise distance} $r_{\min}$ before raising it to the power, computing
\begin{equation}
    F_{p,q} = \left(\frac{r_{\min}}{\norm{p-q}_2}\right)^{s+1} (p - q).
\end{equation}
This is exactly the original force multiplied by the per-iteration constant $r_{\min}^{\,s+1}$, so it leaves the \emph{relative} forces (and hence the dynamics) unchanged --- it merely behaves like an adaptive step size.
The nearest pair now has a ratio of $\approx 1$ so its force stays $O(1)$ for any $s$, while more distant pairs decay towards zero as $s$ grows --- precisely the desired $s \rightarrow \infty$ behaviour.
This is computed by \texttt{pairForce}; the self-interaction term ($p = q$, giving a zero denominator) contributes no force.

\begin{code}
-- repulsive Riesz s-force on point `p` due to neighbour `q`. Distances are
-- rescaled by `ref` (the current minimum pairwise distance) for stability.
pairForce :: Int -> Double -> Vec -> Vec -> Vec
pairForce s ref p q
    | s <= 0    = error "pairForce: s must be a positive integer"
    | r == 0    = replicate (length p) 0   -- same point: no self-force
    | otherwise = scaleVec ((ref / r) ^ (s + 1)) d
  where
    d = vsub p q
    r = l2 d
\end{code}

The reference distance is found by \texttt{minPairDist}, the smallest $L_2$ distance over all distinct pairs of points.

\begin{code}
-- minimum pairwise L2 distance of a set of points
minPairDist :: [Vec] -> Double
minPairDist vs =
    minimum [ l2 (vsub a b)
            | (i, a) <- numbered
            , (j, b) <- numbered
            , i < (j :: Int) ]
  where numbered = zip [0 ..] vs
\end{code}

The total force on a single point is the sum of the pair forces from every point (its own term is zero, so it can be left in the sum).
\texttt{sumVecs} adds a list of vectors component-wise.

\begin{code}
-- component-wise sum of a list of vectors
sumVecs :: [Vec] -> Vec
sumVecs = map sum . transpose

-- total force on point `p` from all points `qs`, rescaled by `ref`
forceOn :: Int -> Double -> [Vec] -> Vec -> Vec
forceOn s ref qs p = sumVecs (map (pairForce s ref p) qs)
\end{code}

\subsubsection{Euler Physics Update}
Euler's method was chosen over Runge-Kutta due to the compositional nature of Haskell, allowing for an easier implementation.
Euler's method for this case is
\begin{subequations}
    \begin{equation}
        v(t+1) = v(t) + a \cdot dt
    \end{equation}
    \begin{equation}
        p(t+1) = p(t) + v(t+1) \cdot dt
    \end{equation}
\end{subequations}
where $a$ is an acceleration, $v(t)$ is a velocity at iteration $t$ and $p(t)$ is a position at iteration $t$.
The mass of each point is taken to be unity, so the acceleration equals the force.
The velocity is updated first (and clamped to a maximum speed of $0.5$), then the position is advanced and clamped to lie within the unit ball.

\begin{code}
-- one Euler time step over the whole simulation
step :: Sim -> Sim
step sim = sim { particles = map update (particles sim) }
  where
    s    = sExp sim
    h    = dt sim
    locs = map pos (particles sim)
    ref  = minPairDist locs               -- rescale distances for stability
    update p = Particle
        { pos = clampNorm 1.0 (vadd (pos p) (scaleVec h v'))
        , vel = v'
        }
      where
        a  = forceOn s ref locs (pos p)      -- mass = 1, so a = F
        v' = clampNorm 0.5 (vadd (vel p) (scaleVec h a))
\end{code}

\subsubsection{Running the Simulation}
Running the simulation iterates \texttt{step} $n$ times.
The naive \texttt{iterate step sim !!\ n} is lazy, which over thousands of iterations builds up a large chain of unevaluated thunks (a space leak).
Instead \texttt{runSim} forces each intermediate state to normal form with \texttt{forceSim} before continuing, so memory use stays flat.

\begin{code}
runSim :: Int -> Sim -> Sim
runSim n = go n
  where
    go 0 sim = sim
    go k sim = go (k - 1) $! forceSim (step sim)

-- force every coordinate of a Sim, so no thunks are left behind
forceSim :: Sim -> Sim
forceSim sim = foldr seq sim
    [ x | p <- particles sim, x <- pos p ++ vel p ]
\end{code}

\subsection{Display Code}
The quantity being maximised is the minimum pairwise $L_2$ distance, computed by \texttt{minDistance} over all distinct pairs.

\begin{code}
-- minimum pairwise distance --- the value we are trying to maximise
minDistance :: [Particle] -> Double
minDistance = minPairDist . map pos
\end{code}

The result is plotted directly in Haskell.
For the two-dimensional case the plot shows the green unit circle (the boundary of the ball), each constellation point, and a blue `packing circle' of radius \texttt{minDistance / 2} around every point: when these circles just touch, the packing is locally optimal.
Both axes are fixed to the same range and the image is rendered square so that circles are not distorted.
The helper \texttt{circle} samples a circle as a closed polyline.

\begin{code}
-- a circle of radius r about (cx, cy), as a closed polyline
circle :: Double -> (Double, Double) -> [(Double, Double)]
circle r (cx, cy) =
    [ (cx + r * cos t, cy + r * sin t) | t <- [0, pi / 60 .. 2 * pi] ]

plotSim :: FilePath -> Sim -> IO ()
plotSim file sim = toFile opts file $ do
    layout_title .= "Thomson packing (min distance = " ++ show md ++ ")"
    layout_x_axis . laxis_generate .= scaledAxis def (-1.1, 1.1)
    layout_y_axis . laxis_generate .= scaledAxis def (-1.1, 1.1)
    setColors [opaque green, opaque blue, opaque red]
    plot (line "unit ball"      [circle 1.0 (0, 0)])
    plot (line "packing radius" [circle (md / 2) c | c <- pts])
    plot (points "points" pts)
  where
    opts = def & fo_size .~ (600, 600)
    pts  = map (to2D . pos) (particles sim)
    md   = minDistance (particles sim)

-- project a vector onto its first two coordinates for 2-D plotting
to2D :: Vec -> (Double, Double)
to2D (x : y : _) = (x, y)
to2D _           = error "to2D: need at least two dimensions to plot"
\end{code}

\subsection{Main}
The entry point runs a small example --- 16 points in the 2-ball with a large Riesz exponent ($s = 20$), which the distance rescaling now makes stable --- and both prints the achieved minimum distance and writes the plot.

\begin{code}
main :: IO ()
main = do
    sim <- initSim 2 16 0.05 20
    let final = runSim 10000 sim
    putStrLn $ "min pairwise distance: "
               ++ show (minDistance (particles final))
    plotSim "thomson.svg" final
    putStrLn "wrote thomson.svg"
\end{code}

\subsection{Debug Code}
For reproducible tests, \texttt{seededSim} builds an initial state from a fixed random seed, so it is pure (no \texttt{IO}) and produces the same points on every run.
\texttt{debugRun} then runs the simulation on that deterministic state.

\begin{code}
-- deterministic initial state from a fixed seed (no IO)
seededSim :: Int -> Int -> Double -> Int -> Sim
seededSim d k dt' s =
    Sim dt' s [ Particle p (replicate d 0) | p <- chunksOf d coords ]
  where
    coords = take (d * k) (randomRs (-0.5, 0.5) (mkStdGen 42))

-- run a reproducible simulation, free of the IO monad
debugRun :: Int -> Int -> Double -> Int -> Int -> Sim
debugRun d k dt' s numIter = runSim numIter (seededSim d k dt' s)
\end{code}

\section{Results}
{\color{red}Discussion here..}

\end{document}
