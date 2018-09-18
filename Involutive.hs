module Involutive (
    symIndList, isAreaOrdered, areaDofList

) where
    
    import Index
    import Tensor
    import Data.List
    import qualified Data.Map.Strict as Map
    import Data.Tuple

    --the first step is extracting the values stored in a tensor in convenient form

    --compute the indlist of n totally symmetric indices ranging from 0 to j

    symIndList :: Int -> Int -> [[Int]]
    symIndList n j 
            | n <= 0 = error "wrong number of indices"
            | n == 1 = [ [a] | a <- [0..j] ]
            | otherwise = [ a ++ [b] | a <- (symIndList (n-1) j), b <- [(last a)..j] ] 

    isAreaOrdered :: [Int] -> Bool
    isAreaOrdered [a,b,c,d] 
            | a < b && a < c && c < d = True
            | a < b && a == c && c < d && b<=d = True
            | otherwise = False
    isAreaOrdered l
            | length l /= 4 = error "wrong list length"
            | otherwise = error "shoul be matched with first block" 

    areaDofList :: [[Int]]
    areaDofList = [ [a,b,c,d] | a <- [0..3], b <- [0..3], c <- [0..3], d <- [0..3], isAreaOrdered([a,b,c,d]) ]
        