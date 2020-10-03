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
    (<>) Colourless Any = Any
    (<>) Any Colourless = Any
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


instance Monoid Colour where
       mempty = Colourless  
       mappend x1 x2 = (<>) x1 x2


class Monoid a where  
    mempty :: a  
    mappend :: a -> a -> a  
    mconcat :: [a] -> a

--Q17.2
data Events = Events [String]

data Probs = Probs [Double]

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2 
    where nToAdd = length l2
          repeatedL1 = map (take nToAdd . repeat) l1
          newL1 = mconcat repeatedL1 cycledL2 = cycle l2

combineEvents :: Events -> Events -> Events
combineEvents (Events e1) (Events e2) = Events (cartCombine combiner e1 e2) 
    where combiner = (\x y -> mconcat [x,"-",y])
    
instance Semigroup Events 
    where  (<>) = combine Events
    
instance Monoid Events 
    where  mappend = (<>)  
           mempty = Events []

combineProbs :: Probs -> Probs -> Probs
combineProbs (Probs p1) (Probs p2) = Probs (cartCombine (*) p1 p2)

instance Semigroup Probs
    where  (<>) = combine Probs
    
instance Monoid Probs
    where  mappend = (<>)  
           mempty = Probs []