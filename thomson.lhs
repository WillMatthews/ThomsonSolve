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

\title{An Iterative Riesz s-Energy Based Solution to Achieving High Maximum Minimum Distance Between Points in the $n$-Ball.}
\date{April 2019}
\author{William Matthews\\ Masters Undergraduate of Engineering Science, University of Oxford}



\begin{document}
\maketitle

\section{Disclaimer}
This was all written by an engineering undergraduate. Exercise reasonable caution and do not believe anything you see here written.

\section{Problem Definition and Use Cases}
Research has revealed high dimensional `one hot vector' codebooks for communications have had little work in their optimisation.
As noise in a communications system corrupts symbols, the most robust BER performance is obtained by \emph{maximising minimum $L_2$ distance between constellation points}.
One sense of defining an optimal codebook $\mathcal{C}_{\text{optimal}}$ is
\begin{equation}
    \mathcal{C}_{\text{optimal}} = \arg \max_{\mathcal{C} \subset \mathfrak{S}} \min_{X_{i}, X_{j} \in \mathcal{C}, i \neq j} \norm {X_i - X_j}_2
\end{equation}
where $\mathfrak{S}$ is the space avaliable to the transmission scheme.

Due to the work on my fourth year project, an algorithm was developed for the case of the ($n-1$)-sphere and $n$-ball.
This developed algorithm is concerned with the $n$-ball as the volume offers more space for point placement and resembles a \emph{symbol power limit}.
Solutions were found to exist for point spacing on the ($n-1$)-sphere, so this work was primarily concerned in spacing inside the $n$-ball
\emph{Future work} will be modifying this existing code to work with 2-ball cartesian products (independant channel power limit).

\emph{No work has been done on Gray Coding the developed constellation, and is a problem left to be solved.}

\section{Algorithm}
The code for this project is written in Haskell (a pure functional language) to ensure safe code.
While Haskell is a high level language the program still performs adequately highly due Haskell being a compiled language.

This section details the code in its entirety and highlights any important steps.
A notation detail is that the red hooked arrow {\textcolor{red}{$\hookrightarrow$} denotes text on the same line but wrapped.

%-- REMINDER the structure is ((dt, s),[([point],[velocity])]) 

\subsection{Imports}
Only the \texttt{System.Random}\footnote{Installable by doing \texttt{cabal install random}} and \texttt{Data.List.Split}\footnote{Installable by doing \texttt{cabal install split}} modules are needed from outside the base.

{\color{red}plotting\footnote{Installable by doing \texttt{cabal install chart-diagrams}}  }

\begin{code}
import System.Random
import Data.List
import Data.List.Split
-- import something_for_plotting
\end{code}



\subsection{Vector Operations}
Some general vector operations are defined since the functionality doesn't natively exist in Haskell.
The $L_2$ norm, vector subtraction, vector normalisation and multiplying a vector by a constant are defined below.
A `scaled' $L_2$ norm is defined to ensure numerical stability when later used with the Riesz s-Energy method later for high $s$.
\begin{code}
-- vec subtraction:  point - loc
vecDiff :: [Float] -> [Float] -> [Float]
vecDiff point loc = zipWith(-) point loc

-- L2 norm
l2 :: [Float] -> Float
l2 diffs = sqrt(sum(map (^2) diffs))

-- TODO fix this - make an appt scaling method
-- 'scaled' L2 norm for numerical stability
scaledL2 :: [Float] -> Int -> Float
scaledL2 diffs s = (l2 diffs)  * (fromIntegral s) * log (fromIntegral s)

normaliseVec :: [Float] -> [Float]
normaliseVec  vec = map(/ (l2 vec)) vec

-- normalise if L2 norm of vec is greater than a constant
normIfGtr :: [Float] -> Float -> [Float]
normIfGtr vecToTest testFloat
    | l2(vecToTest) > testFloat = map (testFloat *) (normaliseVec vecToTest)
    | otherwise                 = vecToTest

multListOfListByConst :: Float -> [[Float]] -> [[Float]]
multListOfListByConst multFact listIn = [map (multFact *) subList | subList <- listIn] 
\end{code}

\subsection{Initialisation Code}
Random points are generated in the \texttt{makeRandPos} function which returns a list of vectors, wrapped in the IO monad.
Zero velocity initial conditions are set with \texttt{makeZeroVelocity} function which returns a list of vectors with all their elements as zero.
A single constellation point has its velocity and position packaged as a \emph{tuple} in \texttt{makePointTup}, and this is evaluated over the list of all points in \texttt{makeAllPointTups} to get a list of tuples.
Care is taken to ensure the IO monad is outside of the datatype.

Finally, the random collection of points with zero velocities is packaged by \texttt{makeState} into a structure that is suitable for repeated composition of the transition function later.
The variables \texttt{dt} and \texttt{s} are placed into this structure which are vital for the transition function structure, and again care is taken to keep the IO outside of the datatype.
\begin{code}
-- initialise velocity - zero for all 'd' dimensions
makeZeroVelocity :: Int -> [Float]
makeZeroVelocity d = take d [0.0,0.0..]

-- initialise - randomly place 'k' points in 'd' dimensions
makeRandPos :: Int -> IO [Float]
makeRandPos d = sequence [randomRIO (-0.5, 0.5) | cnt <- [1..d]]

-- initialise data structure for single point ( 'd' dimensional location and velocity terms )
makePointTup :: Int -> IO ([Float],[Float])
makePointTup d = makeRandPos d >>= \x -> return (x,makeZeroVelocity d)

-- initialise data structure for all 'k' points
makeAllPointTups :: Int -> Int -> IO [([Float],[Float])]
makeAllPointTups k d = sequence [makePointTup d | cnt <- [1..k]]

makeState :: Int -> Int -> Float -> Int -> IO ((Float, Int),[([Float],[Float])])
makeState d k dt s = (makeAllPointTups k d) >>= \x -> return ((dt,s),x)
\end{code}

\subsection{The Transition Function}
The data type for the transition function $g(\cdot)$ to be applied on was \\ \texttt{((Float,Int),[([Float],[Float])])}, with each variable being \\ \texttt{((dt, s),[([point],[velocity])])}.
The transition function was designed to return an identical type such that $g(\cdot)$ can be composed $n$ times,\\ $a_n = g(g(\cdots (a_0) \cdots )) $.
The transition function updates the locations of the points by calculating a `force' between points, and using an Euler's method physics simulation for a single time step.

\subsubsection{Force Function}
The program begins with calculating the force using the Riesz s-energy method for a single pair of points.
The `force function' on a point $p_i$ from another point $p_k$ with the Riesz s-energy method is 
\begin{equation}
    F_{p_i,p_k} = \frac{p_k - p_i}{\norm{p_k-p_i}_2^{s+1}}
\end{equation}
for some $s \in \mathbb{N}$. $F_{p_i,p_k}$ is calculated by \texttt{getSinglePairForce}.
For the equispacing case, $s$ is driven to infinity to make the closest point dominate, but this is not possible due to numerical instability, so $s \approx 20$ and above can be used to gain a good approximation.
A way of visualising the $s \rightarrow \infty$ case is each point having a hard sphere around it - and once points are equispaced the spheres (of identical radius) touch their neighbours.
\begin{code}
getSinglePairForce :: [Float] -> [Float] -> Int -> [Float]
getSinglePairForce point loc s
    | s <= 0    = error "Negative or zero s is illegal"
    |otherwise =  map (/ (scaledL2(vecDiff point loc) s)^(s+1)) (vecDiff point loc)
\end{code}

Using \texttt{getSinglePairForce} and applying over the list of all points, all the force vectors for a single point can be calculated in \texttt{getAllSinglePairForces} and summed to a single force vector in \texttt{getForceSinglePoint}.
\begin{code}
getAllSinglePairForces :: [Float] -> [[Float]] -> Int -> [[Float]]
getAllSinglePairForces point allPoints s = [getSinglePairForce point loc s | loc <- allPoints, loc /= point]

-- sum up all pair forces for a SINGLE POINT
getForceSinglePoint :: [Float] -> [[Float]] -> Int -> [Float]
getForceSinglePoint point allPoints s = map sum . transpose $ (getAllSinglePairForces point allPoints s)
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
The acceleration is calculated from Newton's Second Law in \texttt{getAccelerations}, which uses the forces on all points from \texttt{getForces}.

\texttt{getNewVelocities} updates the velocities first, so the position update in \texttt{updateAllPoints} can occur.

\begin{code}
-- obtain forces for ALL POINTS
getForces :: [[Float]] -> Int -> [[Float]]
getForces allPoints s = [getForceSinglePoint activePoint allPoints s | activePoint <- allPoints]

-- obtain accelerations f/m = a, for this case 'm' is unity. TODO check if this needs changing
getAccelerations :: [[Float]] -> Int -> [[Float]]
getAccelerations allPoints s = getForces allPoints s

extractPoints ::  [([Float],[Float])] -> [[Float]]
extractPoints inTupList = [x | (x,y) <- inTupList]

extractVelocities :: [([Float],[Float])] -> [[Float]]
extractVelocities inTupList = [y | (x,y) <- inTupList]

extractPointStruct :: ((Float, Int),[([Float],[Float])]) -> [([Float],[Float])]
extractPointStruct (a,b) = b


-- update all velocities THEN positions - STANDARD EULER METHOD
-- v = v_old + a * dt
getNewVelocities :: [([Float],[Float])] -> Float -> Int -> [[Float]]
getNewVelocities pointStruct dt s = [normIfGtr (zipWith(+) x y) 1.0 | (x,y) <- (zip (extractVelocities pointStruct) (multListOfListByConst dt (getAccelerations (extractPoints pointStruct) s)))]

-- x = x_old + v * dt
updateAllPoints :: [([Float],[Float])] -> Float -> Int -> [([Float],[Float])]
updateAllPoints pointStruct dt s = [((normIfGtr ((zipWith(+) x (map (dt *) y))) 1.0),y) | (x,y) <- (zip (extractPoints pointStruct) (getNewVelocities pointStruct dt s))]
\end{code}

\subsubsection{Repackaging Variables - The Transition Function}
Finally, the transition function can be fully defined, which takes the structure made earlier in \texttt{makeStruct} and returns an identical datatype which the transitition function can be applied to again.
The transition function uses the method in \texttt{updateAllPoints} to apply the Euler method update to all the constellation points.
\begin{code}
--                  dt     s       pointstruct
transFunction :: ((Float, Int),[([Float],[Float])]) -> ((Float, Int),[([Float],[Float])])
transFunction ((x,y),b) = ((x,y), updateAllPoints b x y)

-- Run Iterator With Random Start to Trans Function
runIterator :: Int -> Int -> Float -> Int -> Int -> IO ((Float, Int),[([Float],[Float])])
runIterator d k dt s numIter = makeState d k dt s >>= \x -> return ((take (numIter+1) (iterate transFunction x)) !! numIter)
\end{code}

\subsection{Display Code}

{\color{red} TODO}

-- plotting code here

\subsection{Debug Code}
\emph{Debug Code} was written to allow for example tests to be generated to aid algorithm development.
\texttt{debugAllPoint}  generates a pure example set of random points.
\texttt{debugTestStruct} generates a pure example `state' structure.
\texttt{debugPointStruct} generates a pure list of points with velocities. 
\texttt{debugShowRandomPoints} allows for an impure random points to be displayed to \texttt{stdout}.
\texttt{debugTest} runs a pure simulation, safe from any IO monad wrapper.

\begin{code}
debugAllPoint :: Int -> Int -> [[Float]]
debugAllPoint d k
    | d < 0     = error "Negative dimensions are not allowed"
    | k < 0     = error "Negative points is not allowed"
    | otherwise = chunksOf d ( take (d*k) (randomRs (-0.5,0.5) (mkStdGen 42))  ) 

debugTestStruct :: Float -> Int -> Int -> Int -> ((Float,Int),[([Float],[Float])])
debugTestStruct dt s d k = ((dt,s), zip (debugAllPoint d k) ([makeZeroVelocity d | cnt <- (debugAllPoint d k)]) )

debugPointStruct :: Int -> Int -> [([Float],[Float])]
debugPointStruct d k = zip (debugAllPoint d k) ([makeZeroVelocity 2 | cnt <- (debugAllPoint d k)]) 

-- debug code - show some random starting positions
debugShowRandomPoints :: Int -> Int -> IO ()
debugShowRandomPoints a b = makeAllPointTups a b >>= print

debugTest :: Float -> Int -> Int -> Int -> Int -> ((Float,Int),[([Float],[Float])])
debugTest dt s d k numIter = (take (numIter+1) (iterate transFunction (debugTestStruct dt s d k))) !! numIter

showTest :: Int -> Int -> Float -> Int -> Int -> IO ()
showTest d k dt s numIter = (runIterator d k dt s numIter) >>= print 

\end{code}


\section{Results}
{\color{red}Discussion here..}

\end{document}
