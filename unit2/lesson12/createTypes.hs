patientInfo :: String -> String -> Int -> Int -> String
patientInfo fname lname age height = name ++ " " ++ ageHeight 
    where name = lname ++ ", " ++ fname       
          ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

type PatientName = (String,String)

-- patientInfo1 :: (Show a1, Show a2) => [Char] -> a1 -> a2 -> [Char]
patientInfo1 :: PatientName -> Int -> Int -> String
patientInfo1 patientName age height = patientName ++ " " ++ ageHeight 
    where  ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"
