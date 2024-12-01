module RQS (quickSort) where

import System.Random

choosePivotRandomly :: (Ord a) => [a] -> Int -> ([a], a)
choosePivotRandomly xs 0 = ([], head xs)
choosePivotRandomly xs len =
    let pureGen = mkStdGen 137
        idx = fst $ uniformR (1 :: Int, len :: Int) pureGen
    in (take idx xs ++ drop (idx+1) xs, xs !! idx)

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort xs = quickSort l ++ [pivot] ++ quickSort r
    where
        (xs', pivot) = choosePivotRandomly xs (length xs - 1)
        l = [a | a <- xs', a <= pivot]
        r = [b | b <- xs', b > pivot]