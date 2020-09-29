-- patientInfo :: String -> String -> Int -> Int -> String
-- patientInfo fname lname age height = name ++ " " ++ ageHeight 
--     where name = lname ++ ", " ++ fname       
--           ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

-- type PatientName = (String,String)

-- -- patientInfo1 :: (Show a1, Show a2) => [Char] -> a1 -> a2 -> [Char]
-- patientInfo1 :: PatientName -> Int -> Int -> String
-- patientInfo1 (lname, fname) age height = name ++ " " ++ ageHeight 
--     where name = lname ++ ", " ++ fname       
--           ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"


data Sex = Male | Female
sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

data RhType = Pos | Neg
data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RhType

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"
showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"
showBloodType :: BloodType -> String
showBloodType (BloodType abo rh)  = showABO abo ++ showRh rh


canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False --otherwise


type FirstName = String
type LastName = String
type MiddleName = String
data Name = Name FirstName LastName           
          | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l 

data Patient = Patient Name Sex Int Int Int BloodType
-- Name: Name
-- Sex: Sex
-- Age (years): Int
-- Height (inches): Int
-- Weight (pounds): Int
-- Blood type: BloodType
johnDoe :: Patient
johnDoe = Patient (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

janeESmith :: Patient
janeESmith = Patient (NameWithMiddle "Jane" "Elizabeh" "Smith") Female 39 70 150 (BloodType O Pos)


getName :: Patient -> Name
getName (Patient n _ _ _ _ _) = n 
getAge :: Patient -> Int
getAge (Patient  _ _ a _ _ _) = a
getBloodType :: Patient -> BloodType
getBloodType (Patient _ _ _ _ _ bt) = bt


--record syntax benefits
--1. easier to understand
--2. easier to create data
--3. Order not impt
--4. Bye to getters for each value permutation
--5. Easier to update

data Patient' = Patient' {    name :: Name 
                            , sex :: Sex   
                            , age :: Int
                            , height :: Int
                            , weight :: Int
                            , bloodType :: BloodType }

jackieSmith :: Patient'
jackieSmith = Patient' {name = Name "Jackie" "Smith"
                      , age = 43
                      , sex = Female
                      , height = 62
                      , weight = 115
                      , bloodType = BloodType O Neg }

-- >>> height jackieSmith
-- 62
-- >>> showBloodType (bloodType jackieSmith)
-- "O-"
-- >>> bloodType jackieSmith  -- fail bcos need the function showBloodType otherwise bloodType (=BloodType O Neg) are just arguments and not function output. 
-- <interactive>:2097:2-22: error:
--     • No instance for (Show BloodType) arising from a use of ‘print’
--     • In a stmt of an interactive GHCi command: print it
--
-- >>> name jackieSmith -- fail. See explanation. Name "Jackie" "Smith" are merely arguments. we still need function output
-- <interactive>:3634:2-17: error:
--     • No instance for (Show Name) arising from a use of ‘print’
--     • In a stmt of an interactive GHCi command: print it
-- >>> showName (name jackieSmith)
-- "Jackie Smith"
--

-- update fields easily 
jackieSmithUpdated :: Patient'
jackieSmithUpdated = jackieSmith { age = 44 }

-- >>> age jackieSmith
-- 43
-- >>> age jackieSmithUpdated
-- 44
--

--Q12.1

canDonate :: Patient' -> Patient' -> Bool
canDonate patient1 patient2 = canDonateTo (bloodType patient1) (bloodType patient2)

--Q12.2

sexType :: Sex -> String
sexType Male = "Male"
sexType Female = "Female"

patientSummary :: Patient' -> String
patientSummary patient = "**************\n" 
                      ++ "Patient Name: " ++ showName (name patient) ++ "\n"
                      ++ "Sex: " ++ sexType (sex patient) ++ "\n"
                      ++ "Age: " ++ show (age patient) ++ "\n"
                      ++ "Height: " ++ show (height patient) ++ " in. \n"
                      ++ "Weight: " ++ show (weight patient) ++ " lbs.\n" 
                      ++ "Blood Type: " ++ showBloodType (bloodType patient) ++ "\n"       
                      ++ "**************\n"               
-- >>> patientSummary jackieSmith
-- "**************\nPatient Name: Jackie Smith\nSex: Female\nAge: 43\nHeight: 62 in. \nWeight: 115 lbs.\nBlood Type: O-\n**************\n"
--
