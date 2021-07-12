{-# LANGUAGE OverloadedStrings #-}
import System.IO                                -- q1
import System.Environment                       -- q1
-- import qualified Data.ByteString as B           -- q1
import qualified Data.ByteString.Char8 as BC    -- q1
import qualified Data.Text as T                 -- q1
-- import qualified Data.Text.IO as TI             -- q1
import qualified Data.Text.Encoding as E        -- q1

-- Q25.1    Write a program that reads in a text file and outputs the difference between the number of characters in the file and the number of bytes in the file.

-- getCounts :: T.Text -> (Int,Int)  - fail bcos mistmatch type
-- getCounts input = (charCount, byteCount)
--         where charCount = (T.length. E.decodeUtf8) input
--               byteCount = BC.length input

main :: IO ()
main = do

-- book solution
            args <- getArgs
            let fileName = args !! 0  -- or can be head args
            input <- BC.readFile fileName
            putStrLn "Bytes: "
            print (BC.length input)
            putStrLn "Characters: "
            print ((T.length. E.decodeUtf8) input)  -- decodeUtf8 :: BC.ByteString -> T.Text
            putStrLn "Difference: "
            print (BC.length input - (T.length. E.decodeUtf8) input)

-- fail bcos mistmatch type
            -- args <- getArgs
            -- let fileName = args !! 0
            -- input <- BC.readFile fileName
            -- putStrLn "Bytes: "
            -- print (fst getCounts)
            -- putStrLn "Characters: "
            -- print (snd getCounts)
            -- -- putStrLn "Difference: "
            -- -- print (charCount - byteCount)

-- my solution with let statements & head args
            args <- getArgs
            let fileName = head args -- or can be:  args !! 0
            input <- BC.readFile fileName
            let byteCount = BC.length input
            let charCount = ((T.length. E.decodeUtf8) input) -- decodeUtf8 :: BC.ByteString -> T.Text
            putStrLn "Bytes: "
            print (byteCount)
            putStrLn "Characters: "
            print (charCount)
            putStrLn "Difference: "
            print (byteCount - charCount)


