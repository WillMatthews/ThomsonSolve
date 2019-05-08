import System.Random
import Data.List


-------------- debug constell in 2D ------------------

debugAllPoint :: Float -> Float -> [[Float]]
debugAllPoint maxVal sep
    | maxVal <= 0  = error "Negative or zero max value not allowed"
    | sep    <= 0  = error "Negative or zero separation not allowed"
    | otherwise   = [ [a,b] | a <- [0.0,sep..maxVal], b <- [0.0,sep..maxVal]]


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
normIfGtr :: [Float] -> [Float]
normIfGtr vecToTest
    | l2(vecToTest) > 1 = normaliseVec vecToTest
    | otherwise         = vecToTest

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

--------------- transition function -------------------

-- TODO make a force float a Maybe Float - can be infinity?

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


-- update all velocities THEN positions - STANDARD EULER METHOD
-- v = v_old + a * dt
-- x = x_old + v * dt

getNewVelocities :: [([Float],[Float])] -> Float -> Int -> [[Float]]
getNewVelocities pointStruct dt s = [ zipWith(+) (fst into) (snd into) | into <- (zip (extractVelocities pointStruct) ( multListOfListByConst dt (getAccelerations (extractPoints pointStruct) s)))]

updateAllPoints :: [([Float],[Float])] -> Float -> Int -> [([Float],[Float])]
updateAllPoints pointStruct dt s = [  ( (zipWith(+) (fst inTup) (map (dt *) (snd inTup))) , (snd inTup) )  | inTup <- (zip (extractPoints pointStruct) (getNewVelocities pointStruct dt s))]




-- TODO apply transition function over and over


---------------- display functions --------------------


-- plotting code here
