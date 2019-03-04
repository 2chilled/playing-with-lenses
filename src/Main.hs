{-# LANGUAGE TemplateHaskell, FlexibleContexts, DuplicateRecordFields, TypeApplications #-}

module Main where
import           Control.Lens
import           Data.Monoid                    ( (<>) )
import           Data.Maybe                     ( isJust )

data Gender = Male | Female deriving (Eq, Show)

newtype Course = Course {
  name :: String
} deriving (Eq, Show)

data Student = Student {
  name :: String,
  courses :: [Course],
  gender :: Gender
} deriving (Eq, Show)

makeLensesFor [
    ("courses", "_stuCourses")
  , ("gender", "_stuGender")
  , ("name", "_stuName")
    ] ''Student

data Prof = Prof {
  name :: String,
  courses :: [Course]
} deriving (Eq, Show)

traverseCourses :: Traversal' Student Course
traverseCourses = _stuCourses . each

studentsOfProfTraversal
  :: (Each s t Student Student, Applicative f)
  => Prof
  -> (Student -> f Student)
  -> s
  -> f t
studentsOfProfTraversal prof =
  --let profCourses' = (courses :: Prof -> [Course]) prof
  let profCourses' = courses (prof :: Prof)
  in  each . filtered
        (isJust . findOf (_stuCourses . each) (`elem` profCourses'))

studentsOfProf :: Prof -> [Student] -> [Student]
studentsOfProf prof = toListOf (studentsOfProfTraversal prof)

main :: IO ()
main =
  let
    courses' = Course <$> ["a", "b", "c"]
    mat      = Student { name = "Matthias", courses = courses', gender = Male }
    tina     = Student { name    = "Tina"
                       , courses = Course <$> ["x", "e"]
                       , gender  = Female
                       }
    bernd = Student { name    = "Bernd"
                    , courses = Course <$> ["a", "e"]
                    , gender  = Male
                    }
    gust = Student { name    = "Gustav"
                   , courses = Course <$> ["b", "e"]
                   , gender  = Male
                   }
    prof = Prof { name = "Prof. X", courses = Course <$> ["d", "e"] }
  in
    do
      print mat
      let students = [mat, bernd, gust, tina]
      putStrLn $ "Students of prof: " <> show (studentsOfProf prof students)
      putStrLn "Adding a course to all students of prof"
      let added = over (studentsOfProfTraversal prof . _stuCourses)
                       (<> [Course "new one"])
                       students
      putStrLn $ "Result: " <> show added
      let added2 =
            students
              &  (studentsOfProfTraversal prof . _stuCourses)
              %~ (<> [Course "new one"])
      putStrLn $ "Inlined: " <> show added2
      putStrLn "Adding a postfix to the name of all male students of prof"
      let postfixAdded = students & over
            ( studentsOfProfTraversal prof
            . filtered (\student -> Male == view _stuGender student)
            . _stuName
            )
            (<> "!MALE!")
      print $ show postfixAdded
