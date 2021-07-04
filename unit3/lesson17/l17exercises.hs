-- Q17.1
import Data.Semigroup

data Color = Red
         |   Yellow
         |   Blue
         |   Green
         |   Purple
         |   Orange
         |   Brown
         |   Colourless deriving (Show,Eq)

instance Semigroup Color where
    (<>) Colourless anyColor = anyColor
    (<>) anyColor Colourless = anyColor
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


instance Monoid Color where
       mempty = Colourless
       mappend col1 col2 = (<>) col1 col2
-- >>>


-- class Monoid a where  -- Dont need this otherwise it means I am creating a new my own class
--     mempty :: a
--     mappend :: a -> a -> a
--     mconcat :: [a] -> a

--Q17.2 similar to exercise in combineFn but Events and Probs are now data types (ie with data constructors), make them instances of Semigroup and Monoid, where combineEvents and combineProbs were the <> operator in each case
data Events = Events [String]  -- include data  contstuctor now

data Probs = Probs [Double]    -- include data  contstuctor now

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
    where nToAdd = length l2
          repeatedL1 = map (take nToAdd . repeat) l1
          newL1 = mconcat repeatedL1
          cycledL2 = cycle l2

-- note the addition of `Events` data constructors now
combineEvents :: Events -> Events -> Events
combineEvents (Events e1) (Events e2) = Events (cartCombine combiner e1 e2)
    where combiner = (\x y -> mconcat [x,"-",y])

-- note the removal of `Events` data constructors now in (<>)
instance Semigroup Events where
      (<>) = combineEvents

-- note the removal of `Events` data constructors now in (<>)
instance Monoid Events where
     mappend = (<>)
     mempty = Events []

-- note the addition of `Probs` data constructors now
combineProbs :: Probs -> Probs -> Probs
combineProbs (Probs p1) (Probs p2) = Probs (cartCombine (*) p1 p2)

-- note the removal of `Probs` data constructors now in (<>)
instance Semigroup Probs
    where  (<>) = combineProbs

-- note the removal of `Probs` data constructors now in (<>)
instance Monoid Probs where
    mappend = (<>)
    mempty = Probs []

