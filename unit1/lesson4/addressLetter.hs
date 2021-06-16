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


getLocationFunction :: [Char] -> ([Char], [Char]) -> [Char]
getLocationFunction location  = case location of
    "ny" -> nyOffice
    "sf" -> sfOffice
    "reno" -> renoOffice
    _ -> (\name -> (fst name) ++ " " ++ (snd name))


addressLetter :: ([Char], [Char]) -> [Char] -> [Char]
addressLetter name location = locationFunction name
    where locationFunction = getLocationFunction location

-- >>> addressLetter ("Bob","Smith") "ny"
-- >>> addressLetter ("Bob","Jones") "ny"
-- >>> addressLetter ("Samantha","Smith") "sf"
-- >>> addressLetter ("Bob","Smith") "reno"
-- >>> addressLetter ("Bob","Smith") "la"
-- "Bob Smith: PO Box 789 - New York, NY, 10013"
-- "Bob Jones: PO Box 789 - New York, NY, 10013"
-- "Samantha Smith - PO Box 1010 - San Francisco, CA, 94109"
-- "Smith - PO Box 456 - Reno, NV 89523"
-- "Bob Smith"
--
