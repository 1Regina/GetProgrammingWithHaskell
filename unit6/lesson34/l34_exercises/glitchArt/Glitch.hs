module Glitch (glitchActionswReverse) where
import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.Random ( randomRIO )
import Control.Monad ( foldM )

-- main :: IO ()
-- main = do
--             args <- getArgs
--             let fileName = head args
--             imageFile <- BC.readFile fileName
--             glitched <- foldM (\bytes func -> func bytes) imageFile glitchActionswReverse
--             let glitchedFileName = mconcat ["glitched_",fileName]
--             BC.writeFile glitchedFileName glitched
--             print "all done"

-- Steps to execute:
-- 1. ghc glitch.hs
-- 2. ./glitch lovecraft.jpg -- ../../../../unit4/lesson25/lovecraft.jpg

glitchActionswReverse :: [BC.ByteString -> IO BC.ByteString]
glitchActionswReverse = [randomReplaceByte  -- technique A
                        ,randomSortSection  -- technique B
                        ,randomReplaceByte  -- technique A
                        ,randomSortSection  -- technique B
                        ,randomReplaceByte  -- technique A
                        ,randomReverseBytes] -- technique C

-- technique A
intToChar :: Int -> Char
intToChar int =  toEnum safeInt
    where safeInt = int `mod` 255

intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

--  code to replace a byte with a value with replaceByte function
replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc charVal bytes = mconcat [before,newChar,after]
    where (before,rest) = BC.splitAt loc bytes
          after = BC.drop 1 rest
          newChar = intToBC charVal

-- apply replaceByte to the location to change and the value to change to
randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
    let bytesLength = BC.length bytes
    location <- randomRIO (1,bytesLength)
    charVal <- randomRIO (0,255)
    return (replaceByte location charVal bytes)

-- technique B
-- sortSection function, which takes a starting point of the section, a size of the section, and the byte stream. It split, sort the second section of chunks of fixed size and put together.
sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before,changed,after]
    where (before,rest) = BC.splitAt start bytes
          (target,after) = BC.splitAt size rest
          changed =  BC.reverse (BC.sort target)
-- Use IO action to pick a random starting point (Randomize sortSection)
randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
    let sectionSize = 25
    let bytesLength = BC.length bytes
    start <- randomRIO (0,bytesLength - sectionSize)
    return (sortSection start sectionSize bytes)

-- technique C
-- Add another glitching technique, randomReverseBytes, that randomly reverses a section of bytes in your data (reference function is sortSection)
reverseSection :: Int -> Int -> BC.ByteString -> BC.ByteString
reverseSection start size bytes = mconcat [before,changed,after]
                  where (before,rest) = BC.splitAt start bytes
                        (target,after) = BC.splitAt size rest
                        changed =  BC.reverse target

randomReverseBytes :: BC.ByteString -> IO BC.ByteString
randomReverseBytes bytes = do
    let sectionSize = 25
    let bytesLength = BC.length bytes
    start <- randomRIO (0,(bytesLength - sectionSize))
    return (reverseSection start sectionSize bytes)