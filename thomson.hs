import System.Random
import Data.List
import Data.List.Grouping

-------------- debug constell in 2D ------------------

debugAllPoint :: Int -> Int -> [[Float]]
debugAllPoint d k
    | d < 0     = error "Negative dimensions are not allowed"
    | k < 0     = error "Negative points is not allowed"
    | otherwise = splitEvery d ( take (d*k) (randomRs (-0.5,0.5) (mkStdGen 42))  ) 

debugTestStruct :: Float -> Int -> Int -> Int -> ((Float, Int),[([Float],[Float])])
debugTestStruct dt s d k = ((dt,s), zip (debugAllPoint d k)  ([makeZeroVelocity 2 | cnt <- (debugAllPoint d k)]) )

debugPointStruct :: Int -> Int -> [([Float],[Float])]
debugPointStruct d k = zip  (debugAllPoint d k) ([makeZeroVelocity 2 | cnt <- (debugAllPoint d k)]) 
---------------- vector operations -------------------

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
multListOfListByConst multFact listIn = [ map (multFact *) subList | subList <- listIn ] 


------------------- init code  -----------------------

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

-- debug code - show some random starting positions
showRandomPoints :: Int -> Int -> IO ()
showRandomPoints a b =  makeAllPointTups a b >>= print

--             dims   points   dt   s
makeState :: Int -> Int -> Float -> Int -> IO ((Float, Int),[([Float],[Float])])
makeState d k dt s = (makeAllPointTups k d) >>= \x -> return ((dt,s),x)

--------------- transition function -------------------

-- TODO make a force float a Maybe Float - can be infinity?

-- REMINDER the structure is ((dt, s),[([point],[velocity])]) 

-- for a single PAIR
getSinglePairForce :: [Float] -> [Float] -> Int -> [Float]
getSinglePairForce point loc s
    | s <= 0    = error "Negative or zero s is illegal"
    |otherwise =  map (/ (scaledL2(vecDiff point loc) s)^(s+1)) (vecDiff point loc)

-- obtain list of all forces for a SINGLE POINT from all different pairs point
getAllSinglePairForces :: [Float] -> [[Float]] -> Int -> [[Float]]
getAllSinglePairForces point allPoints s = [getSinglePairForce point loc s | loc <- allPoints, loc /= point]

-- sum up all pair forces for a SINGLE POINT
getForceSinglePoint :: [Float] -> [[Float]] -> Int -> [Float]
getForceSinglePoint point allPoints s = map sum . transpose $ (getAllSinglePairForces point allPoints s)

-- obtain forces for ALL POINTS
getForces :: [[Float]] -> Int -> [[Float]]
getForces allPoints s = [getForceSinglePoint activePoint allPoints s | activePoint <- allPoints]

-- obtain accelerations f/m = a, for this case 'm' is unity. TODO check if this needs changing
getAccelerations :: [[Float]] -> Int -> [[Float]]
getAccelerations allPoints s = getForces allPoints s

extractPoints ::  [([Float],[Float])] -> [[Float]]
extractPoints inTupList = [fst(subTup) | subTup <- inTupList]

extractVelocities :: [([Float],[Float])] -> [[Float]]
extractVelocities inTupList = [snd(subTup) | subTup <- inTupList]

extractPointStruct :: ((Float, Int),[([Float],[Float])]) -> [([Float],[Float])]
extractPointStruct globalStruct = snd globalStruct

-- REMINDER the structure is ((dt, s),[([point],[velocity])]) 


-- update all velocities THEN positions - STANDARD EULER METHOD
-- v = v_old + a * dt
getNewVelocities :: [([Float],[Float])] -> Float -> Int -> [[Float]]
getNewVelocities pointStruct dt s = [ normIfGtr (zipWith(+) (fst into) (snd into)) 1.0 | into <- (zip (extractVelocities pointStruct) ( multListOfListByConst dt (getAccelerations (extractPoints pointStruct) s)))]

-- x = x_old + v * dt
updateAllPoints :: [([Float],[Float])] -> Float -> Int -> [([Float],[Float])]
updateAllPoints pointStruct dt s = [  ((normIfGtr ( (zipWith(+) (fst inTup) (map (dt *) (snd inTup)))) 1.0) , (snd inTup) )  | inTup <- (zip (extractPoints pointStruct) (getNewVelocities pointStruct dt s))]

--                  dt     s       pointstruct
transFunction :: ((Float, Int),[([Float],[Float])]) -> ((Float, Int),[([Float],[Float])])
transFunction globalStruct = ((fst globalStruct), updateAllPoints (snd globalStruct) (fst (fst globalStruct)) (snd (fst globalStruct)))


-- Apply Random Start to Trans Function
--              d      k       dt      s      n
runIterator :: Int -> Int -> Float -> Int -> Int -> IO ((Float, Int),[([Float],[Float])])
runIterator d k dt s numIter = makeState d k dt s >>= \x -> return (((take (numIter+1) (iterate transFunction x)) ) !! numIter)


-- debug code
--showTest :: Int -> Int -> Float -> Int -> Int -> IO ()
--showTest d k dt s numIter = (runIterator d k dt s numIter) !! numIter >>= print 


---------------- display functions --------------------


-- plotting code here
