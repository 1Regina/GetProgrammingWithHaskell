{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Semigroup --to use <> and mconcat to join Text since there is no T.++

helloPerson :: T.Text -> T.Text
helloPerson name = "Hello" <> " " <> name <> "!"

main :: IO ()
main = do
    TIO.putStrLn "Hello! What's your name?"
    name <- TIO.getLine
    let statement = helloPerson (name)
    TIO.putStrLn statement

-- Steps
--         1.  ghc --make l23_1exercises.hs
--         2.  ./l23_1exercises
--         3.  [DONT NEED] <ctdl-d> to end and get results of computation



