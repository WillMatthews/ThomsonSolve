import System.Random

-- initialise - randomly place 'k' points in 'd' dimensions
makeZeroVelocity :: Int -> [Float]
makeZeroVelocity d = take d [0.0,0.0..]

makeRandPos :: Int -> IO [Float]
makeRandPos d = sequence [randomRIO (-0.5, 0.5) | cnt <- [1..d]]

makePointTup :: Int -> (IO [Float],[Float])
makePointTup d = (makeRandPos d, makeZeroVelocity d) -- position & velocity in 'd' dimensions

makeAllPointTups :: Int -> Int -> [(IO [Float],[Float])]
makeAllPointTups k d = [makePointTup d | cnt <- [1..k]] 

showRandomPoints :: IO ()
showRandomPoints = do  -- Doesn't work - stupid
    putStrLn "Random Points Result"
    let x = makeAllPointTups 3 3
    toprint <- sequence x
    mapM_ print x


-- WHAT GOES HERE TO PRINT MY LIST OF IOs?


--------------- transition function-------------------

-- getforce (single point)

-- getallforces

-- getaccelerations

-- movepoints


-- TODO apply transition function over and over


---------------- display functions--------------------


-- plotting?
