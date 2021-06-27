-- Q13.1
-- >>> :info Word
-- data Word = GHC.Types.W# GHC.Prim.Word# 	-- Defined in ‘GHC.Types’
-- instance Eq Word -- Defined in ‘GHC.Classes’
-- instance Ord Word -- Defined in ‘GHC.Classes’
-- instance Enum Word -- Defined in ‘GHC.Enum’
-- instance Num Word -- Defined in ‘GHC.Num’
-- instance Real Word -- Defined in ‘GHC.Real’
-- instance Show Word -- Defined in ‘GHC.Show’
-- instance Read Word -- Defined in ‘GHC.Read’
-- instance Bounded Word -- Defined in ‘GHC.Enum’
-- instance Integral Word -- Defined in ‘GHC.Real’

-- >>> minBound::Word
-- >>> maxBound :: Word
-- >>> minBound
-- >>> maxBound
-- 0
-- 18446744073709551615
-- ()
-- ()
--
-- >>> :info Int
-- data Int = GHC.Types.I# GHC.Prim.Int# 	-- Defined in ‘GHC.Types’
-- instance Eq Int -- Defined in ‘GHC.Classes’
-- instance Ord Int -- Defined in ‘GHC.Classes’
-- instance Enum Int -- Defined in ‘GHC.Enum’
-- instance Num Int -- Defined in ‘GHC.Num’
-- instance Real Int -- Defined in ‘GHC.Real’
-- instance Show Int -- Defined in ‘GHC.Show’
-- instance Read Int -- Defined in ‘GHC.Read’
-- instance Bounded Int -- Defined in ‘GHC.Enum’
-- instance Integral Int -- Defined in ‘GHC.Real’
---- >>> minBound::Int
---- -9223372036854775808
----
-- >>> maxBound :: Int
-- >>> minBound
-- >>> maxBound
-- 9223372036854775807
-- ()
-- ()

-- >>> 9223372036854775807 < 18446744073709551615
-- True
--

-- Both word and int are instances of Eq, Ord, Enum, Num, Real, Show, Read, Bounded and Integral type classes
-- The difference is Int has a lower max bound limit and Word min bound is 0 whereas Int can go negative. Hence Word is an Int that takes only positive values

-- Q13.2
-- >>> :info Enum
-- class Enum a where
--   succ :: a -> a
--   pred :: a -> a
--   toEnum :: Int -> a
--   fromEnum :: a -> Int
--   enumFrom :: a -> [a]
--   enumFromThen :: a -> a -> [a]
--   enumFromTo :: a -> a -> [a]
--   enumFromThenTo :: a -> a -> a -> [a]
--   {-# MINIMAL toEnum, fromEnum #-}
--   	-- Defined in ‘GHC.Enum’
-- instance Enum Word -- Defined in ‘GHC.Enum’
-- instance Enum Ordering -- Defined in ‘GHC.Enum’
-- instance Enum Integer -- Defined in ‘GHC.Enum’
-- instance Enum Int -- Defined in ‘GHC.Enum’
-- instance Enum Char -- Defined in ‘GHC.Enum’
-- instance Enum Bool -- Defined in ‘GHC.Enum’
-- instance Enum () -- Defined in ‘GHC.Enum’
-- instance Enum Float -- Defined in ‘GHC.Float’
-- instance Enum Double -- Defined in ‘GHC.Float’

-- GHCi> succ maxBound :: Int
-- *** Exception: Prelude.Enum.succ{Int}: tried to take ‘succ' of maxBound

inc :: Int -> Int
inc x = x + 1

-- Both inc and succ gives the next element (to the right). However inc can only be applied exclusively to Int type, meaning it cannot be applied to char like alphabets; succ however can be applied to alphabets

-- >>> inc 'c'
-- <interactive>:4316:6-8: error:
--     • Couldn't match expected type ‘Int’ with actual type ‘Char’
--     • In the first argument of ‘inc’, namely ‘'c'’
--       In the expression: inc 'c'
--       In an equation for ‘it’: it = inc 'c'
--
-- >>> succ  'c'
-- 'd'
--
-- Answer
-- GHCi> inc maxBound :: Int-9223372036854775808
-- GHCi> succ maxBound :: Int*** Exception: Prelude.Enum.succ{Int}: tried to take ‘succ' of maxBound
-- Because there’s no true successor to a Bounded type, succ throws an error. The inc function just rotates you back to the beginning.

-- Q13.3 to keep recycling
cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n = if n == maxBound
              then minBound
              else succ n

-- >>> maxBound :: Int
-- 9223372036854775807

-- >>> cycleSucc '1'
-- '2'

-- >>> cycleSucc 'p'
-- 'q'

