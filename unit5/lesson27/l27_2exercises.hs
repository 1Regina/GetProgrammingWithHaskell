{-# LANGUAGE OverloadedStrings #-}
import System.IO
import System.Environment
-- Q27.3    Write a command-line interface for partsDB that lets the user look up the cost of an item, given an ID. Use the Maybe type to handle the case of the user entering missing input.

import qualified Data.Map as Map -- 1. for implementation of Map.fromList

-- 2. RobotPart defined using record syntax
data RobotPart = RobotPart
    { name :: String
    , description :: String
    , cost :: Double
    , count :: Int
    } deriving Show
-- Example robot parts: leftArm, rightArm, and robotHead
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

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals   -- to make into a map aka dictionary after zip.
    where keys = [1,2,3]
          vals = [leftArm,rightArm,robotHead]
          keyVals = zip keys vals

showCost :: Maybe Double -> IO()
showCost Nothing = putStrLn  "No such item. Try again"
showCost (Just cost) = print cost

-- >>> cost robotHead
-- 5092.25

main :: IO ()
main = do
    putStrLn "enter ID of item"
    itemID <- getContents
    let part = Map.lookup (read itemID) partsDB --find the item
    showCost (fmap cost part)    -- extract info wrt the item


-- Steps to run
-- 1. ghc --make l27_2exercises.hs
-- 2. ./l27_2exercises
-- 3. follow IO command to input ID range (1,3)
-- 4. <Ctrl d>
-- 5. output is shown