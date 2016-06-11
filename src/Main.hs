{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

module Main where
import Control.Lens
import Data.Monoid ((<>))
import Data.Maybe (isJust, isNothing)
import Debug.Trace (trace)

data Course = Course {
  _couName :: String
} deriving (Eq, Show)

makeLenses ''Course

data Student = Student {
  _stuName :: String,
  _stuCourses :: [Course]
} deriving (Eq, Show)

makeLenses ''Student

data Prof = Prof {
  _profName :: String,
  _profCourses :: [Course]
} deriving (Eq, Show)

makeLenses ''Prof

traverseCourses :: Traversal' Student Course
traverseCourses = stuCourses . each

studentsOfProfTraversal prof = 
  let profCourses = _profCourses prof
  in each . filtered (isJust . findOf (stuCourses . each) (`elem` profCourses)) 

studentsOfProf :: Prof -> [Student] -> [Student]
studentsOfProf prof = toListOf (studentsOfProfTraversal prof) 

main :: IO ()
main = 
  let courses = Course <$> ["a", "b", "c"]
      mat = Student {
        _stuName = "Matthias",
        _stuCourses = courses
      }
      bernd = Student {
        _stuName = "Bernd",
        _stuCourses = Course <$> ["a", "e"]
      }
      gust = Student {
        _stuName = "Gustav",
        _stuCourses = Course <$> ["b", "e"]
      }
      prof = Prof {
        _profName = "Prof. X",
        _profCourses = Course <$> ["d", "e"]
      }
  in do
   print mat
   let students = [mat, bernd, gust]
   putStrLn $ "Students of prof: " <> show (studentsOfProf prof students)
   putStrLn "Adding a course to all students of prof"
   let added = over (studentsOfProfTraversal prof . stuCourses) (<> [Course "new one"]) students
   putStrLn $ "Result: " <> show added
   let added2 = students & (studentsOfProfTraversal prof . stuCourses) %~ (<> [Course "new one"])
   putStrLn $ "Inlined: " <> show added2
