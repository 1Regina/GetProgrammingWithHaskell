import Data.Array.Unboxed

-- UArrays take two type parameters; the first is for the type of the index, and the second is for the type of the value. For Int types, this is 0; and for Bools, it’s False

aLargeList :: [Int]
aLargeList = [1 .. 10000000]

aLargeArray :: UArray Int Int
aLargeArray = array (0,9999999) []

aLargeListDoubled :: [Int]
aLargeListDoubled = map (*2) aLargeList


-- 1. zeroIndexArray
-- parameter 1: lower and upper bounds for your indices.
-- parameter 2: list of (index, value) pairs.
zeroIndexArray :: UArray Int Bool
zeroIndexArray = array (0,9) [(3,True)]

-- look up values in your UArray by using the ! operator
-- >>> zeroIndexArray ! 5
-- False
-- >>> zeroIndexArray ! 3
-- True
-- >>> zeroIndexArray
-- array (0,9) [(0,False),(1,False),(2,False),(3,True),(4,False),(5,False),(6,False),(7,False),(8,False),(9,False)]

-- 2. 1-indexed array (with all the Bools set to True). zip function combined with cycle to generate a list of value pairs that are all True
oneIndexArray :: UArray Int Bool
oneIndexArray = array (1,10) $ zip [1 .. 10] $ cycle [True]
-- >>> oneIndexArray ! 1
-- True
-- >>> oneIndexArray ! 10
-- True

-- if you try to access an element outside your index bounds, you’ll get an error
-- >>> oneIndexArray ! 0
-- Ix{Int}.index: Index (0) out of range ((1,10))
-- >>> oneIndexArray
-- array (1,10) [(1,True),(2,True),(3,True),(4,True),(5,True),(6,True),(7,True),(8,True),(9,True),(10,True)]

-- Quick check 42.1     Create an array with the following signature:
qcArray :: UArray Int Bool
-- The array contains five elements indexed at 0 and the 2, and three elements are set to True
-- rem default Bool value is false
qcArray = array (0,4) $ zip [0 .. 2] $ cycle [True]
-- >>> qcArray ! 5
-- >>> qcArray ! 4
-- >>> qcArray ! 3
-- >>> qcArray ! 2
-- >>> qcArray ! 1
-- >>> qcArray ! 0
-- Ix{Int}.index: Index (5) out of range ((0,4))
-- False
-- False
-- True
-- True
-- True
qcArray' :: UArray Int Bool
qcArray' = array (0,4) [(1,True),(2,True)]
-- >>> qcArray' ! 5
-- >>> qcArray' ! 4
-- >>> qcArray' ! 3
-- >>> qcArray' ! 2
-- >>> qcArray' ! 1
-- >>> qcArray' ! 0
-- Ix{Int}.index: Index (5) out of range ((0,4))
-- False
-- False
-- True
-- True
-- False

-- 42.1.3 UArray can be updated like any functional data structure, by creating a copy of it with the appropriate value changes.
-- sammple Case of 4 buckets for beans
beansInBuckets :: UArray Int Int
beansInBuckets = array (0,3) []
-- Because you passed in an empty list of pairs for initial values, your UArray has been initialized to zeros (default value for Int)
-- >>> beansInBuckets ! 0
-- >>> beansInBuckets ! 3
-- 0
-- 0

-- >>> beansInBuckets
-- array (0,3) [(0,0),(1,0),(2,0),(3,0)]

-- Quick check 42.2 Rather than assume your values will be initialized to zeros, make sure of it
beansInBuckets' :: UArray Int Int
beansInBuckets' = array (0,3) $ zip  [0 ..3] $ cycle [0]

-- To add five beans to bucket 1 and six beans to bucket 3 (with bucket 0 being your first bucket) by using the (//) operator. The
-- first argument to (//) is UArray, (beansInBucket)
-- second argument is a new list of pairs.
-- The result is a new UArray with the updated values

updatedBiB :: UArray Int Int
updatedBiB = beansInBuckets // [(1,5),(3,6)]
-- >>> updatedBiB ! 1
-- >>> updatedBiB ! 2
-- >>> updatedBiB ! 3
-- 5
-- 0
-- 6
-- >>> updatedBiB
-- array (0,3) [(0,0),(1,5),(2,0),(3,6)]

-- 42.7 Now to add two beans to every bucket. Use the accum function which takes a binary function, a UArray, and a list of values to apply the function to.
-- >>> accum (+) updatedBiB $ zip [0 .. 3] $ cycle [2]
-- array (0,3) [(0,2),(1,7),(2,2),(3,8)]

-- >>> zip [0 .. 3] $ cycle [2]
-- [(0,2),(1,2),(2,2),(3,2)]

-- >>> accum (+) updatedBiB [(0,2),(1,2),(2,2),(3,2)]
-- array (0,3) [(0,2),(1,7),(2,2),(3,8)]

-- Quick check 42.3     Try doubling the number of beans in each bucket.
-- >>> accum (*) updatedBiB $ zip [0..3] $ cycle [2]
-- array (0,3) [(0,0),(1,10),(2,0),(3,12)]
