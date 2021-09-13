import Data.Array.Unboxed -- to use UArray
import Data.Array.ST -- to use `STUArray`
import Control.Monad -- use `STUArray`
import Control.Monad.ST -- use `STUArray`

-- Step 1: put values in array
listToSTUArray1  :: [Int] -> ST s (STUArray s Int Int)
listToSTUArray1 vals = do
    let end =  length vals - 1
    stArray <- newArray (0,end) 0  -- stArray is a mutable array in a context assigned with <-.
    return stArray
-- >>> listToSTUArray [1,2,3]
-- <<ST action>>

-- Step 2a use forM_ from Control.Monad. The forM_ action takes your data and a function to apply to the data as arguments. This has the nice property of replicating a forin loop in languages such as Python.
-- Step 2b Then use a list of indices and (!!) to look up values in your list
-- Step 2c write the values from the list to your stArray value with writeArray function, which takes an STUArray, an index, and your value. writeArray function writes a stateful mutation of your underlying array and doesn’t create a copy of it.

listToSTUArray :: [Int] -> ST s (STUArray s Int Int)
listToSTUArray vals = do
    let end =  length vals - 1
    myArray <- newArray (0,end) 0
    forM_ [0 .. end] $ \i -> do
        let val = vals !! i
        writeArray myArray i val
    return myArray

-- Step 3 Take value out of STUArray context. Because STUArray is enforcing encapsulation (objects properly hide all of their implementation details from the user), you’re not constrained by the same limitations of IO. You can take values out of STUArray by using a function named runSTUArray
-- runSTUArray :: ST s (STUArray s i e) -> UArray i

-- listToUArray, that uses stateful programming but appears to be a pure function.
listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ listToSTUArray vals
-- >>> listToUArray [1,2,3]
-- array (0,2) [(0,1),(1,2),(2,3)]

-- Step 4: Common Haskell without intermediary function (listToSTUArray)
listToUArrayGold :: [Int] -> UArray Int Int
listToUArrayGold vals = runSTUArray $ do
    let end =  length vals - 1
    myArray <- newArray (0,end) 0
    forM_ [0 .. end] $ \i -> do
        let val = vals !! i
        writeArray myArray i val
    return myArray
-- >>> listToUArrayGold [1,2,3]
-- array (0,2) [(0,1),(1,2),(2,3)]
