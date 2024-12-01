module DayOne (diff_distance, similarity_score) where

import qualified RQS (quickSort)

needle :: (Ord a, Ord b, Num a, Num b) => a -> [(a, b)] -> Maybe b
needle _ [] = Nothing
needle k (x:xs) = 
    if fst x == k then
        Just $ snd x
    else
        needle k xs

increaseVal :: (Ord a, Ord b, Num a, Num b) => a -> [(a, b)] -> [(a, b)]
increaseVal k kv =
    let val = needle k kv in
        case val of
            Nothing -> kv
            Just v -> (k, v+1) : [x | x <- kv, fst x /= k]  

count_appearences :: (Eq a, Ord a, Ord b, Num a, Num b) => [a] -> [(a, b)] -> [(a, b)]
count_appearences [] kv = kv
count_appearences (x:xs) kv = count_appearences xs (increaseVal x kv) 


find_number_elements :: (Ord a, Num a) => [a] -> [(a, a)] -> [a]
find_number_elements [] _ = []
find_number_elements (x:xs) kv = 
    let freq = needle x kv in
        case freq of
            Nothing -> x*0 : (find_number_elements xs kv)
            Just v -> x*v : (find_number_elements xs kv)

diff_distance :: (Num a, Ord a) => [a] -> [a] -> [a]
diff_distance xs ys = [abs $ x - y | (x, y) <- zip (RQS.quickSort xs) (RQS.quickSort ys)]

similarity_score :: (Num a, Ord a) => [a] -> [a] -> a
similarity_score xs ys =
    let init_kv = zip ys $ repeat 0
        kvdb = count_appearences ys init_kv
        similarity = find_number_elements xs kvdb 
    in
        sum similarity
    

