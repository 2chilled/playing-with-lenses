data Lens a b = Lens {get :: a -> b, set :: b -> a -> a}

compose :: Lens a b -> Lens b c -> Lens a c
compose (Lens get1 set1) (Lens get2 set2) = Lens (get2 . get1) (\c a -> set1 (set2 c $ get1 a) a)

(|.|) = compose

data Address = Address {street :: String, zip :: Integer} deriving (Show)

data Student = Student {studentname :: String, address :: Address} deriving (Show)

data Professor = Professor {professorname :: String} deriving (Show)

data Coach = Coach {topic :: String, prof :: Professor, student :: Student} deriving (Show)

coachToStudent :: Lens Coach Student
coachToStudent = Lens student (\s c -> Coach (topic c) (prof c) s)

studentToAddress :: Lens Student Address
studentToAddress = Lens address (\a s -> Student (studentname s) a)

coachToAddress :: Lens Coach Address
coachToAddress = coachToStudent |.| studentToAddress
