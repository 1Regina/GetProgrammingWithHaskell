import Data.List ()
--Q1

compareLastName :: (Ord a1, Ord a2) => (a1, a2) -> (a1, a2) -> Ordering
compareLastName name1 name2 = if result == EQ
                              then compare (fst name1) (fst name2)
                              else result
    where result = compare (snd name1) (snd name2)

-- >>> compareLastName ("Peter","Hook") ("Stephen","Morris")
-- LT
--

-- >>> compareLastName  ("Ian", "Curtis") ("Bernard","Sumner")
-- LT
--

-- >>> snd ("Amy" , "Grant")
-- "Grant"
--

-- >>> compare "aN" "BY"
-- GT
--
-- >>> compareLastName  ("Ian", "Curtis") ("Bernard","Curtis")
-- GT



--Q2
sfOffice :: ([Char], [Char]) -> [Char]
sfOffice name = if lastName < "L"
                then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111"
                else nameText ++ " - PO Box 1010 - San Francisco, CA, 94109"
        where lastName = snd name
              nameText = (fst name) ++ " " ++ lastName
nyOffice :: ([Char], [Char]) -> [Char]
nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"  where nameText = (fst name) ++ " " ++ (snd name)
renoOffice :: (a, [Char]) -> [Char]
renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"  where nameText = snd name
wdcOffice :: ([Char], [Char]) -> [Char]
wdcOffice name = nameText ++ " Esq" ++ " - PO Box 123 - Washington DC, DC 12345"  where nameText = (fst name) ++ " " ++ (snd name)
getLocationFunction :: [Char] -> ([Char], [Char]) -> [Char]
getLocationFunction location = case location of
    "ny" -> nyOffice
    "sf" -> sfOffice
    "reno" -> renoOffice
    "wdc" -> wdcOffice
    _ -> (\name -> (fst name) ++ " " ++ (snd name))


addressLetter :: ([Char], [Char]) -> [Char] -> [Char]
addressLetter name location = locationFunction name
    where locationFunction = getLocationFunction location

-- >>> addressLetter ("Bob","Smith") "wdc"
-- "Bob Smith Esq - PO Box 123 - Washington DC, DC 12345"
--
