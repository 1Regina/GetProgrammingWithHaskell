
-- import qualified Data.List
-- -- Q27.1    When we introduced parameterized types in lesson 15, you used a minimal type Box as the example:
data Box a = Box a deriving Show

-- a) Implement the Functor type class for Box.
-- b) Then implement morePresents, which changes a box from type Box a to one of type Box [a], which has n copies of the original value in the box in a list. Make sure to use fmap to implement this.
-- Part a)
instance Functor Box where
     fmap func (Box x)  = Box (func x)

-- Part b) MorePresents when val is a Int, Char or Strings
morePresents :: (Num a, Enum a) => a -> Box b -> Box [b]
-- morePresents n (Box val ) =  Box (map (\n -> val) [1..n])
morePresents n (Box val ) = Box ((\n -> val) <$> [1..n] ) -- criteria is fmap

-- >>> morePresents 3 (Box 7)
-- Box [7,7,7]

-- >>> morePresents 3 (Box 'C')
-- Box "CCC"
-- >>> Box ['C','C','C']
-- Box "CCC"

-- >>> morePresents 3 (Box "Cats")
-- Box ["Cats","Cats","Cats"]


-- Ref(https://stackoverflow.com/questions/19459728/basic-haskell-list-comprehension-for-copying-an-element-n-times#:~:text=copy2%20x%20y%20%3D%20map%20(%5Cx%20-%3E%20y)%20%5B1..x%5D)
copy2 :: (Num a, Enum a) => a -> b -> [b]
copy2 x y = map (\x -> y) [1..x]

-- >>> copy2 3 7
-- [7,7,7]
-- >>> (replicate 3) ['c','a']
-- ["ca","ca","ca"]


-- QC27.2    Now suppose you have a simple box like this:
myBox :: Box Int
myBox = Box 1
-- Use fmap to put the value in your Box in another Box. Then define a function unwrap that takes a value out of a box, and use fmap on that function to get your original box.
-- Here’s how your code should work in GHCi:

-- GHCi> wrapped = fmap ? myBox
-- GHCi> wrapped
-- Box (Box 1)
-- GHCi> fmap unwrap wrapped
-- Box 1

unwrap :: Box a -> a
unwrap (Box val) = val

-- My experiment
-- wrapped :: a -> Box a
-- wrapped x = Box x
-- >>> wrapped myBox
-- Box (Box 1)
-- >>> wrapped <$> myBox
-- Box (Box 1)
-- >>> fmap wrapped myBox
-- Box (Box 1)
-- >>> fmap unwrap wrapped myBox
-- Box 1

-- My solution that matched requirements
wrap :: Box (Box Int)
wrap = fmap Box (myBox)
-- >>> wrap
-- Box (Box 1)
-- >>> fmap unwrap wrap
-- Box 1

