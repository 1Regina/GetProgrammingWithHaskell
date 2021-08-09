module Main where
-- import qualified Data.Text as T
-- import Data.Text.IO as TIO ( getLine )
import Glitch ( glitchActionswReverse )
-- import qualified Glitch as Art

import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.Random ( randomRIO )
import Control.Monad ( foldM )


main :: IO ()
main = do
            args <- getArgs
            let fileName = head args
            imageFile <- BC.readFile fileName
            glitched <- foldM (\bytes func -> func bytes) imageFile glitchActionswReverse
            let glitchedFileName = mconcat ["glitched_",fileName]
            BC.writeFile glitchedFileName glitched
            print "all done"



-- -- Steps:
-- --1.  ghc Main.hs
-- --2. ./Main