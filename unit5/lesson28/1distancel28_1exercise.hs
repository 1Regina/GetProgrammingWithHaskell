import qualified Data.Map as Map -- step 1

-- Program that do:
-- 1. get two locations from your locationDB
-- 2. calculate their distance, and
-- 3. then pass that distance to printDistance.

-- 2. Using a Map as your database of city coordinates
type LatLong = (Double,Double)
locationDB :: Map.Map String LatLong
locationDB = Map.fromList [("Arkham",(42.6054,-70.7829))
                          ,("Innsmouth",(42.8250,-70.8150))
                          ,("Carcosa",(29.9714,-90.7694))
                          ,("New York",(40.7776,-73.9691))]

-- 3. Distance between two points on the globe from your locationDB with Haversine formula and convert latitude and longitude to radians first.
toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRads :: LatLong -> (Double,Double)
latLongToRads (lat,long) = (rlat,rlong)
    where rlat = toRadians lat
          rlong = toRadians long

haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
    where (rlat1,rlong1) = latLongToRads coords1
          (rlat2,rlong2) = latLongToRads coords2
          dlat = rlat2 - rlat1
          dlong = rlong2 - rlong1
          a = (sin (dlat/2))^2 + cos rlat1 * cos rlat2 * (sin (dlong/2))^2
          c = 2 * atan2 (sqrt a) (sqrt (1-a))
          earthRadius = 3961.0

-- Distance between New York and Innsmouth
-- >>> haversine (40.7776,-73.9691) (42.6054,-70.7829)
-- 207.3909006336738

-- 4. Command line tool for user to enter in two city names, and you’ll return the distance.
-- goal : an IO action that takes a Maybe value for your distance and either prints the distance or tells the user that an error occurred.
printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "Error, invalid city entered"
printDistance (Just distance) = putStrLn (show distance ++ " miles")

-- but locationDB will give you Maybe values but haversine :: LatLong  -> LatLong  -> Double and should be

-- Create wrapper function for working in a Maybe cause other problems
-- 1. wrapper for any similar function ---> repetitive
-- 2. a different `haversineMaybe` for other context e.g `IO`.
haversineMaybe :: Maybe LatLong -> Maybe LatLong -> Maybe Double
haversineMaybe Nothing _ = Nothing
haversineMaybe _ Nothing = Nothing
haversineMaybe (Just val1) (Just val2) = Just (haversine val1 val2)

-- Quick check 28.1 Write addMaybe for adding two Maybe Ints.
addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe (Just number1) (Just number2) = Just (number1 + number2)
-- addMaybe Nothing _ = Nothing
-- addMaybe _ Nothing = Nothing
addMaybe _ _  = Nothing -- summary of case addMaybe Nothing _ and case addMaybe _ Nothing


-- Quick check 28.2    Suppose    you    don’t    have    to    worry    about    Maybes and have raw coordinatepairs. If you have the pair newYork, how would you make a function distanceFromNY that’s waiting for an additional location?

-- newYork :: (Double, Double)
-- distanceFromNY ::  (Double, Double) -> (Double, Double)
-- distanceFromNY coordsCity = haversine newYork coordsCity

maybeInc :: Maybe (Integer -> Integer)
maybeInc = (+) <$> Just 1
-- >>> maybeInc 2  -- out of context
-- Couldn't match expected type ‘Integer -> t’
--             with actual type ‘Maybe (Integer -> Integer)’

-- >>> maybeInc <*> Just 5
-- Just 6
-- >>> maybeInc <*> Nothing
-- Nothing
-- >>> (++) <$> Just "cats" <*> Just " and dogs"
-- Just "cats and dogs"
-- >>> (++) <$> Nothing <*> Just " and dogs"
-- Nothing
-- >>> (++) <$> Just "cats" <*> Nothing
-- Nothing

-- Quick check 28.3    Use the pattern for using binary values in a context for the functions (*),div, and mod on these two values:
val1 = Just 10
val2 = Just 5
chain1 = (+) <$> val1 <*> val2
chain2 = (div) <$> val1 <*> val2
chain3 = (mod) <$> val1 <*> val2
-- >>> chain1
-- Just 15
-- >>> chain2
-- Just 2
-- >>> chain3
-- Just 0

-- Applicative city distance program
startingCity = Map.lookup "Carcosa" locationDB
destCity = Map.lookup "Innsmouth" locationDB
-- >>> haversine <$> startingCity <*> destCity
-- Just 1415.7942372467567


-- solution with applicative and fmap
main :: IO ()
main = do
    putStrLn "Enter the starting city name:"
    startingInput <- getLine
    let startingCity = Map.lookup startingInput locationDB
    putStrLn "Enter the destination city name:"
    destInput <- getLine
    let destCity = Map.lookup destInput locationDB
    let distance = haversine <$> startingCity <*> destCity
    printDistance distance

-- steps :
-- 1. ghc 1distance.hs
-- 2. ./1distance

-- Q28.1    Writing    haversineMaybe (listing 28.4) was straightforward. Write the function haversineIO without using <*>. Here’s the type signature:

haversineIO :: IO LatLong -> IO LatLong -> IO Double
haversineIO ioVal1 ioVal2 = do
        putStrLn "Enter 2 cities coordinates"
        val1 <- ioVal1
        val2 <- ioVal2
        let distance = haversine val1 val2
        return distance

-- Q28.2    Rewrite    haversineIO, this time using <*>
haversineIO' :: IO LatLong -> IO LatLong -> IO Double
haversineIO' ioVal1 ioVal2 = haversine <$> ioVal1 <*> ioVal2

