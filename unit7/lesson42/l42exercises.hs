import Data.Array.Unboxed -- to use UArray
import Data.Array.ST -- to use `STUArray`
import Control.Monad -- use `STUArray`
import Control.Monad.ST -- use `STUArray`
import PrelNames (crossDataConKey)

-- Q42.1    One of the most important operations in the implementation of a genetic algo-rithm is combining two arrays of Booleans through an operation called crossover. Cross-over takes as input a pair of equal-sized arrays. Then a cutoff point is chosen, and the top and bottom are swapped. The final value is this new pair of arrays. Here’s an illustration using lists and an example (using 1 for True and 0 for False):
-- ([1,1,1,1,1],[0,0,0,0,0])
-- If you perform crossover at index 3, your result should be
-- [1,1,1,0,0]
-- Implement crossover where the result is a UArray but the crossover itself is performed using STUArrays.

crossOver :: (UArray Int Int, UArray Int Int) -> Int -> UArray Int Int
crossOver (array1,array2) crossOverPt  = runSTUArray $ do
    stArray1 <- thaw array1
    let end = (snd . bounds) array1
    forM_ [crossOverPt .. end] $ \i -> do
        writeArray stArray1 i $ array2 ! i
    return stArray1

array1 :: UArray Int Int
array1 = array (0,4) $ zip [0 .. 4] $ cycle [1]
-- >>> array1
-- array (0,4) [(0,1),(1,1),(2,1),(3,1),(4,1)]

array2 :: UArray Int Int
array2 = array (0,4) $ zip [0 .. 4] $ cycle [0]
-- >>> array2
-- array (0,4) [(0,0),(1,0),(2,0),(3,0),(4,0)]

-- >>> crossOver (array1, array2) 3
-- array (0,4) [(0,1),(1,1),(2,1),(3,0),(4,0)]



-- Q42.2    Write a function that takes a `UArray Int Int` as an input. The input will have a mixture of zeros and other values. The function, `replaceZeros`, should return the array with all of the zeros replaced with the value –1.

rawArray :: UArray Int Int
rawArray = array (0,5) [(0,0),(1,3),(2,0),(3,9),(4,4),(5,0)]

replaceZeroes :: UArray Int Int -> UArray Int Int
replaceZeroes array = runSTUArray $ do
    stArray <- thaw array
    let end = (snd . bounds) array
    forM_ [0 .. end] $ \i -> do
        val <- readArray stArray i
        when (val == 0) $ do
            writeArray stArray i (-1)
    return stArray

-- >>> replaceZeroes rawArray
-- array (0,5) [(0,-1),(1,3),(2,-1),(3,9),(4,4),(5,-1)]
