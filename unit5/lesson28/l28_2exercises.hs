-- Q28.3    Recall    the    RobotPart type from the preceding lesson. Make a command-line application that has a database of various RobotParts (at least five), and then lets the user enter in two-part IDs and returns the one with the lowest cost. Handle the case of the user entering an ID that’s not in the parts database.
import qualified Data.Map as Map -- step 1

data RobotPart = RobotPart
    { name :: String
    , description :: String
    , cost :: Double
    , count :: Int
    } deriving Show

--2. Create parts. Example robot parts: leftArm, rightArm, and robotHead, robotLeg, robotTail
leftArm :: RobotPart
leftArm  = RobotPart
    { name = "left arm"
    , description = "left arm for face punching!"
    , cost = 1000.00
    , count = 3
    }
rightArm :: RobotPart
rightArm  = RobotPart
    { name = "right arm"
    , description = "right arm for kind hand gestures"
    , cost = 1025.00
    , count = 5
    }
robotHead :: RobotPart
robotHead  = RobotPart
    { name = "robot head"
    , description = "this head looks mad"
    , cost = 5092.25
    , count = 2
    }
robotLeg :: RobotPart
robotLeg  = RobotPart
    { name = "robot leg"
    , description = "this leg looks symmetrical"
    , cost = 7052.25
    , count = 4
    }
robotTail :: RobotPart
robotTail  = RobotPart
    { name = "robot tail"
    , description = "this tail looks hairy"
    , cost = 888.25
    , count = 5
    }

-- 3 partsDB, which is your internal database of RobotParts. Build a Map is with the fromList function
partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
    where keys = [1,2,3,4,5]
          vals = [leftArm,rightArm,robotHead,robotLeg, robotTail]
          keyVals = zip keys vals

keys = [1,2,3,4,5]
vals = [leftArm,rightArm,robotHead,robotLeg, robotTail]
keyVals = zip keys vals
-- >>> keyVals
-- [(1,RobotPart {name = "left arm", description = "left arm for face punching!", cost = 1000.0, count = 3}),(2,RobotPart {name = "right arm", description = "right arm for kind hand gestures", cost = 1025.0, count = 5}),(3,RobotPart {name = "robot head", description = "this head looks mad", cost = 5092.25, count = 2}),(4,RobotPart {name = "robot leg", description = "this leg looks symmetrical", cost = 7052.25, count = 4}),(5,RobotPart {name = "robot tail", description = "this tail looks hairy", cost = 888.25, count = 5})]

-- 4. Handle non-existent parts in DB
printCost :: Maybe Double -> IO()
printCost Nothing = putStrLn  "this part does not exist"
printCost (Just cost) = print cost


-- 5. comparison IO function
main :: IO ()
main = do
    putStrLn "Enter first RobotPart index you want to compare"
    part1 <- getLine
    putStrLn "Enter second RobotPart index you want to compare"
    part2 <- getLine
    let robotPart1 = Map.lookup (read part1) partsDB
    let robotPart2 = Map.lookup (read part2) partsDB
    let cheaper = min <$> (cost <$> robotPart1) <*> (cost <$> robotPart2)
    printCost cheaper

-- >>> cost robotTail
-- 888.25

-- Steps to run
-- 1. ghc l28_2exercises.hs
-- 2.  ./l28_2exercises
