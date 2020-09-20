--hello.hs my first Haskell file!

main :: IO ()
main = do
    print "Hello World"

testprogram :: IO ()
testprogram = do
    print "Hello World!"


-- >>> testprogram
-- "Hello World!"
--
-- check1 :: IO ()
check1 = do
    print "Hello Me"
