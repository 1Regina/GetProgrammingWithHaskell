import Data.Array.Unboxed -- to use UArray
import Data.Array.ST -- to use `STUArray`
import Control.Monad -- use `STUArray`
import Control.Monad.ST -- use `STUArray`

myData :: UArray Int Int
myData = listArray (0,5) [7,6,4,8,10,2]
-- >>> myData
-- array (0,5) [(0,7),(1,6),(2,4),(3,8),(4,10),(5,2)]

-- Quick check 42.4 Use your listToUArray function to define myData
--recall
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
myData' :: UArray Int Int
myData' = listToUArrayGold [7,6,4,8,10,2]
-- >>> myData'
-- array (0,5) [(0,7),(1,6),(2,4),(3,8),(4,10),(5,2)]

-- Just because you’re in a context doesn’t mean that you can treat UArray as though it were stateful.
-- 1. Use a function called thaw, which will unfreeze your UArray so you can work with it.
-- 2. Use the bounds function, which gives you a pair representing the bounds of your array so you can figure out where it ends.
-- 3. STUArray has a function called readArraythat reads a stateful value from an array.
-- 4. Finally, use an interesting function named when, which works like an if without a then in most programming languages. Here’s the implementation of bubbleSort.

bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort myArray = runSTUArray $ do
    stArray <- thaw myArray
    let end = (snd . bounds) myArray
    forM_ [1 .. end] $ \i -> do
        forM_ [0 .. (end - i)] $ \j -> do
            val <- readArray stArray j
            nextVal <- readArray stArray (j + 1)
            let outOfOrder = val > nextVal
            when outOfOrder $ do
                writeArray stArray j nextVal
                writeArray stArray (j + 1) val
    return stArray

-- >>> bubbleSort myData
-- array (0,5) [(0,2),(1,4),(2,6),(3,7),(4,8),(5,10)]
