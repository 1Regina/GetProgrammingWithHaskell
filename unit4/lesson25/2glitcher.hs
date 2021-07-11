import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.Random ( randomRIO )
import Control.Monad ( foldM )

mainBasic :: IO ()
mainBasic = do
    args <- getArgs
    let fileName = head args
    imageFile <- BC.readFile fileName
    glitched <- return imageFile  --- THE POINT OF GLITCHING
    let glitchedFileName = mconcat ["glitched_",fileName]
    BC.writeFile glitchedFileName glitched
    print "all done"

-- Quick check 25.2    At    this    point,    the    glitched    variable    in    your    main doesn’t need to be an IO type. Change that line so that glitched is a regular variable
-- Ans: let glitched = imageFile



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

mainGlitchLittle :: IO ()
mainGlitchLittle = do
    args <- getArgs
    let fileName = head args
    imageFile <- BC.readFile fileName
    glitched <- randomReplaceByte imageFile  --- THE POINT OF GLITCHING
    let glitchedFileName = mconcat ["glitched_",fileName]
    BC.writeFile glitchedFileName glitched
    print "all done"

-- Steps to execute:after rename mainGlitchLittle to main
-- 1. ghc 2glitcher.hs
-- 2. ./2glitcher lovecraft.jpg

-- Quick check 25.3 Write an IO action that returns a random Char. (See notes 20.4 on toEnum)
-- randomChar :: IO Char
-- randomChar = do
--     randomInt <- randomRIO (0,255)
--     return (toEnum randomInt)

-- To enhance glitchArt (Step 15)
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

-- Step 15 enhanced glitchArt -  replace randomReplaceByte with randomSortSection (here)
mainGlitchBetter :: IO ()
mainGlitchBetter = do
        args <- getArgs
        let fileName = head args
        imageFile <- BC.readFile fileName
        glitched <- randomSortSection imageFile --- THE POINT OF GLITCHING
        let glitchedFileName = mconcat ["glitched_",fileName]
        BC.writeFile glitchedFileName glitched
        print "all done"

-- Steps to execute:after rename mainGlitchBetter to main
-- 1. ghc 2glitcher.hs
-- 2. ./2glitcher lovecraft.jpg

-- Combo GlitchArt : use randomSortSection twice on your data and randomReplaceByte three times.
mainGlitchCombo :: IO ()
mainGlitchCombo = do
            args <- getArgs
            let fileName = head args
            imageFile <- BC.readFile fileName
            glitched1 <- randomReplaceByte imageFile --- THE POINT OF GLITCHING
            glitched2 <- randomSortSection glitched1 --- THE POINT OF GLITCHING
            glitched3 <- randomReplaceByte glitched2 --- THE POINT OF GLITCHING
            glitched4 <- randomSortSection glitched3 --- THE POINT OF GLITCHING
            glitched5 <- randomReplaceByte glitched4 --- THE POINT OF GLITCHING
            let glitchedFileName = mconcat ["glitched_",fileName]
            BC.writeFile glitchedFileName glitched5
            print "all done"

-- Steps to execute:after rename mainGlitchCombo to main
-- 1. ghc 2glitcher.hs
-- 2. ./2glitcher lovecraft.jpg

-- With foldM from Control.Monad (Step 17) to reduce typo and use lambda for chaining.
mainLambdaActions :: IO ()
mainLambdaActions= do
            args <- getArgs
            let fileName = head args
            imageFile <- BC.readFile fileName
            glitched <- foldM (\bytes func -> func bytes) imageFile
                                                                 [randomReplaceByte,randomSortSection,randomReplaceByte,randomSortSection,randomReplaceByte]
            let glitchedFileName = mconcat ["glitched_",fileName]
            BC.writeFile glitchedFileName glitched
            print "all done"


-- Quick  check  25.4    Create    a    variable    glitchActions  outside  your  main  that  includes  all  your actions in a list. Don’t forget to give it the correct type.
-- Ans:
glitchActions :: [BC.ByteString -> IO BC.ByteString]
glitchActions = [randomReplaceByte
                ,randomSortSection
                ,randomReplaceByte
                ,randomSortSection
                ,randomReplaceByte]

-- Step 18. Using stringed glitchActions
main :: IO ()
main = do
            args <- getArgs
            let fileName = head args
            imageFile <- BC.readFile fileName
            glitched <- foldM (\bytes func -> func bytes) imageFile glitchActions
            let glitchedFileName = mconcat ["glitched_",fileName]
            BC.writeFile glitchedFileName glitched
            print "all done"