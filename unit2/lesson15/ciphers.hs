-- encode alphabet to alphabet for even number alphabet
rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
    where halfAlphabet = alphabetSize `div` 2
          offset = fromEnum c + halfAlphabet
          rotation =  offset `mod` alphabetSize

-- encode alphabet to alphabet for even and odd number alphabet
rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation 
    where halfN = n `div` 2       
          offset = if even n
                   then fromEnum c + halfN
                   else 1 + fromEnum c + halfN
          rotation =  offset `mod` n


rotEncoder :: String -> String
rotEncoder text = map rotChar text 
    where alphaSize = 1 + fromEnum (maxBound :: Char)
          rotChar = rotN alphaSize

-- (same as above)
rotDecoder :: String -> String
rotDecoder text =  map rotCharDecoder text 
    where alphaSize = 1 + fromEnum (maxBound :: Char)
          rotCharDecoder = rotNdecoder alphaSize



-- >>> rotEncoder "hi"
-- "\557160\557161"
--

-- >>> rotDecoder(rotEncoder "hi")
-- "hi"
--
