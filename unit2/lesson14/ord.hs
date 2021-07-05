import Data.List
-- Ord type has 3 data constructors (LT, GT, EQ)
data Ordering = LT | GT | EQ

-- Functions involving Ord variables would always be about comparison.
-- class Ord a where
--   compare :: a -> a -> Ordering


-- In this Name type , the Name data constructors takes 2 strings. Name (left one which is a type) is an instance of Show type + Eq type + Ord type (Actually dont need to write Ord as EQ is a superclass of Ord)
newtype Name = Name (String, String) deriving (Show, Eq) -- deriving (Show, Eq, Ord)


-- Since Ord is customized for comparison, I can apply it to Name for Name comparison
instance Ord Name where
       compare (Name (f1,l1)) (Name (f2,l2)) = compare (l1,f1) (l2,f2)
        -- where f1 = fst Name1
        --       l1 = snd Name1
        --       f2 = fst Name2
        --       l2 = snd Name2

names :: [Name]
names = [Name ("Emil","Cioran")
      , Name ("Eugene","Thacker")
      , Name ("Friedrich","Nietzsche")]


-- >>> sort names
-- [Name ("Emil","Cioran"),Name ("Friedrich","Nietzsche"),Name ("Eugene","Thacker")]
