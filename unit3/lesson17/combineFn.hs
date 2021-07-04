import Data.Semigroup

--Quickcheck 17.1 solut
myAny :: (a -> Bool) -> [a] -> Bool
myAny testFunc = (foldr (||) False) . (map testFunc)
-- >>> myAny even [1,2,3]
-- True
--
-- Quickcheck 17.2
instance Semigroup Integer where
    (<>) x y = x + y

-- instance Semigroup Int where
--     (/) x y = x / y -- No, because division doesn’t always return an Int type, which violates the rule. Sometimes could return float.

-- Steps to implement semigroup for colour
-- 1. create color type first
data Color = Red
         |   Yellow
         |   Blue
         |   Green
         |   Purple
         |   Orange
         |   Brown deriving (Show,Eq)  -- had to use Eq bcos its combine 2 colours EQUALS what colour

-- 2. make color an instance of Semigroup so can activate semigroup (<>) to do color mix.
-- instance Semigroup Color where
--     (<>) Red Blue = Purple
--     (<>) Blue Red = Purple
--     (<>) Yellow Blue = Green
--     (<>) Blue Yellow = Green
--     (<>) Yellow Red = Orange
--     (<>) Red Yellow = Orange
--     (<>) a b = if a == b
--                then a
--                else Brown

-- >>> Red <> Yellow
-- Orange

-- associative of colour misisng
-- >>> (Green <> Blue) <> Yellow
-- >>> Green <> (Blue <> Yellow)
-- Brown
-- Green

-- >>> (<>) Red Blue
-- Purple

-- 3. Handle any associativity
instance Semigroup Color where
    (<>) Red Blue = Purple
    (<>) Blue Red = Purple
    (<>) Yellow Blue = Green
    (<>) Blue Yellow = Green
    (<>) Yellow Red = Orange
    (<>) Red Yellow = Orange
    (<>) a b | a == b = a
             | all (`elem` [Red,Blue,Purple]) [a,b] = Purple
             | all (`elem` [Blue,Yellow,Green]) [a,b] = Green
             | all (`elem` [Red,Yellow,Orange]) [a,b] = Orange
             | otherwise = Brown

-- >>> (<>) Red Blue
-- Purple

-- After fixing associativity.
-- >>> (Green <> Blue) <> Yellow
-- Green

-- Quickcheck 17.3
--Yes as addition of Integers are associative.  1 + 3 = 3 = 1 & also   1 + (2 + 3) = (1 + 2) + 3.


-- --Quickcheck 17.4 mempty value for mappend/<> for product ie * instead of addition
-- mappend :: a -> a -> a

-- instance Semigroup Integer where
--     (<>) x y = x * y

-- 1, because x × 1 = x.


--Monoid Example
-- Probability Table -- table of event with their probability

type Events = [String]
type Probs = [Double]

data PTable = PTable Events Probs

-- Rem probability sum to 1
createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
        where totalProbs = sum probs
              normalizedProbs = map (\x -> x/totalProbs) probs

--  PTable needs to be an instance of the Show type class
-- Start by printing 1 event with its probability
showPair :: String -> Double -> String
showPair event prob = mconcat [event,"|", show prob,"\n"]

-- mconcat not only requires less typing, but also provides a preferable way to combine strings.

instance Show PTable where
    show (PTable events probs) = mconcat pairs
        where pairs = zipWith showPair events probs

-- >>> createPTable ["heads","tails"] [0.5,0.5]
-- heads|0.5
-- tails|0.5




-- -- Create a combine probTable -- Cartesian product
-- 1.   cartCombine function for Cartesian product
cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2    -- zipWith :  repeat each element in the first list once for each element in the second.
        where nToAdd = length l2
              repeatedL1 = map (take nToAdd . repeat) l1    -- Maps l1 and makes nToAdd copies of the element. Returns a list of lists
              newL1 = mconcat repeatedL1                    -- join the list of lists from repeated
              cycledL2 = cycle l2                           -- to use zipWith to combine these two lists. cycle the second list

-- 2. combineEvents and combineProbs
combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = cartCombine combiner e1 e2
        where combiner = (\x y -> mconcat [x,"-",y])           -- combine events with "-"

combineProbs :: Probs -> Probs -> Probs
combineProbs p1 p2 = cartCombine (*) p1 p2               -- combine probabilities by multiply them

-- 3. make PTable an instance of Semigroup with combineEvent and combineProbs
instance Semigroup PTable where
    (<>) ptable1 (PTable [] []) = ptable1                  --  special case of having an empty PTable
    (<>) (PTable [] []) ptable2 = ptable2                  -- special case of having an empty PTable
    (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
        where newEvents = combineEvents e1 e2
              newProbs = combineProbs p1 p2

-- 3a. createPTable (done above)
-- createPTable :: Events -> Probs -> PTable
-- createPTable events probs = PTable events normalizedProbs where
--     totalProbs = sum probs
--     normalizedProbs = map (\x -> x/totalProbs) probs

-- 4. make PTable an instance of Monoid. Recall : mappend and <> are the same; so settle the identity -- mempty element aka PTable [] [].
instance Monoid PTable where
    mempty = PTable [] []
    mappend = (<>)

-- 5. example combine a coin and a colour spinner
coin :: PTable
coin = createPTable ["heads","tails"] [0.5,0.5]

spinner :: PTable
spinner = createPTable ["red","blue","green"] [0.1,0.2,0.7]

-- >>> coin <> spinner
-- heads-red|5.0e-2
-- heads-blue|0.1
-- heads-green|0.35
-- tails-red|5.0e-2
-- tails-blue|0.1
-- tails-green|0.35

-- >>> mconcat [coin,coin,coin]
-- heads-heads-heads|0.125
-- heads-heads-tails|0.125
-- heads-tails-heads|0.125
-- heads-tails-tails|0.125
-- tails-heads-heads|0.125
-- tails-heads-tails|0.125
-- tails-tails-heads|0.125
-- tails-tails-tails|0.125

