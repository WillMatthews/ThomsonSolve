\documentclass[11pt,twoside,a4paper]{article}

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
               {<=}{{$\leq$}}2 {=>}{{$\Rightarrow$}}2 
               {\ .}{{$\circ$}}2 {\ .\ }{{$\circ$}}2
               {>>}{{>>}}2 {>>=}{{>>=}}2
               {|}{{$\mid$}}1
    }

\title{An Iterative Riesz s-Energy Solution to Achieving High Maximum Minimum Distance Between Points in the $n$-Ball.}
\date{April 2019}
\author{William Matthews\\ Masters Undergraduate of Engineering Science, University of Oxford}



\begin{document}
\maketitle

\section{Problem Definition and Use Cases}
{\color{red}TODO}
\section{Algorithm}
{\color{red}TODO}

%-- REMINDER the structure is ((dt, s),[([point],[velocity])]) 

\subsection{Imports}
{\color{red}TODO}


\begin{code}
import System.Random
import Data.List
import Data.List.Grouping
\end{code}



\subsection{Vector Operations}
{\color{red}TODO}

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
scaledL2 diffs s = (l2 diffs) -- * s * log(s :: Float)

normaliseVec :: [Float] -> [Float]
normaliseVec  vec = map(/ (l2 vec)) vec

-- normalise if L2 norm of vec is greater than 1
normIfGtr :: [Float] -> Float -> [Float]
normIfGtr vecToTest testFloat
    | l2(vecToTest) > testFloat = map (testFloat *) (normaliseVec vecToTest)
    | otherwise                 = vecToTest

multListOfListByConst :: Float -> [[Float]] -> [[Float]]
multListOfListByConst multFact listIn = [map (multFact *) subList | subList <- listIn] 
\end{code}

\subsection{Initialisation Code}
{\color{red}TODO}

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

\subsubsection{Force Function}
The program begins with calculating the force using the Riesz s-energy method for a single pair of points.
\begin{code}
getSinglePairForce :: [Float] -> [Float] -> Int -> [Float]
getSinglePairForce point loc s
    | s <= 0    = error "Negative or zero s is illegal"
    |otherwise =  map (/ (scaledL2(vecDiff point loc) s)^(s+1)) (vecDiff point loc)
\end{code}


-- obtain list of all forces for a SINGLE POINT from all different pairs point
\begin{code}
getAllSinglePairForces :: [Float] -> [[Float]] -> Int -> [[Float]]
getAllSinglePairForces point allPoints s = [getSinglePairForce point loc s | loc <- allPoints, loc /= point]

-- sum up all pair forces for a SINGLE POINT
getForceSinglePoint :: [Float] -> [[Float]] -> Int -> [Float]
getForceSinglePoint point allPoints s = map sum . transpose $ (getAllSinglePairForces point allPoints s)
\end{code}

\subsubsection{Euler Physics Update}
{\color{red} TODO}

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
{\color{red}TODO}

\begin{code}
--                  dt     s       pointstruct
transFunction :: ((Float, Int),[([Float],[Float])]) -> ((Float, Int),[([Float],[Float])])
transFunction ((x,y),b) = ((x,y), updateAllPoints b x y)

-- Apply Random Start to Trans Function
--              d      k       dt      s      n
runIterator :: Int -> Int -> Float -> Int -> Int -> IO ((Float, Int),[([Float],[Float])])
runIterator d k dt s numIter = makeState d k dt s >>= \x -> return (((take (numIter+1) (iterate transFunction x))) !! numIter)
\end{code}

\subsection{Display Code}

{\color{red} TODO}

-- plotting code here

\subsection{Debug Code}
\emph{Debug Code} was written to allow for example tests to be generated to aid algorithm development

\begin{code}
debugAllPoint :: Int -> Int -> [[Float]]
debugAllPoint d k
    | d < 0     = error "Negative dimensions are not allowed"
    | k < 0     = error "Negative points is not allowed"
    | otherwise = splitEvery d ( take (d*k) (randomRs (-0.5,0.5) (mkStdGen 42))  ) 

debugTestStruct :: Float -> Int -> Int -> Int -> ((Float,Int),[([Float],[Float])])
debugTestStruct dt s d k = ((dt,s), zip (debugAllPoint d k) ([makeZeroVelocity 2 | cnt <- (debugAllPoint d k)]) )

debugPointStruct :: Int -> Int -> [([Float],[Float])]
debugPointStruct d k = zip (debugAllPoint d k) ([makeZeroVelocity 2 | cnt <- (debugAllPoint d k)]) 

-- debug code - show some random starting positions
showRandomPoints :: Int -> Int -> IO ()
showRandomPoints a b = makeAllPointTups a b >>= print

debugTest :: Float -> Int -> Int -> Int -> Int -> ((Float,Int),[([Float],[Float])])
debugTest dt s d k numIter = (take (numIter+1) (iterate transFunction (debugTestStruct dt s d k))) !! numIter

--showTest :: Int -> Int -> Float -> Int -> Int -> IO ()
--showTest d k dt s numIter = (runIterator d k dt s numIter) !! numIter >>= print 

\end{code}



\end{document}
