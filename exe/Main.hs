module Main where

import System.Environment
import qualified DayOne (diff_distance, similarity_score)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      contents <- readFile file
      let cases = lines contents
          numbers = map words cases
          fstLst = [head x | x <- numbers]
          sndLst = [last x | x <- numbers]
          fstLstInt :: [Int]
          fstLstInt = map read fstLst
          sndLstInt :: [Int]
          sndLstInt = map read sndLst
          diffs = DayOne.diff_distance fstLstInt sndLstInt
          sim = DayOne.similarity_score fstLstInt sndLstInt
      putStrLn $ "The sum of differences is: " ++ (show $ sum diffs)
      putStrLn $ "The similarity score is: " ++ (show $ sim)
    _ -> putStrLn "wrong argument"

