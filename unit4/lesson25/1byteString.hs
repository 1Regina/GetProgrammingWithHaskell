{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC  --NOT Data.ByteString bcos to unpack from ByteString to [Char] to then apply as String
import qualified Data.ByteString as B

sampleBytes :: B.ByteString
sampleBytes = "Hello!"

-- B.unpack :: BC.ByteString -> [GHC.Word.Word8]
-- BC.unpack :: BC.ByteString -> [Char]

sampleString :: String
-- sampleString = B.unpack sampleBytes -- FAIL bcos cannot unpack with B.
sampleString = BC.unpack sampleBytes

-- >>> sampleString
-- Variable not in scope: sampleString

-- Quick  check  25.1     Write  a  function  that  takes  numbers  in  ASCII  character  form  and  converts them to Ints. For example, make the following an Int:
bcInt :: BC.ByteString
bcInt = "6"

bcToInt :: BC.ByteString -> Int
bcToInt = read .  BC.unpack


