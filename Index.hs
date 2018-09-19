--the first step for implementing the Tensor data type in Haskell is defining index data types with the correct ranges


module Index (
Uind_a(..), Lind_a(..), Uind_I(..), Lind_I(..), Uind_A(..), Lind_A(..),
Uinds_a, Linds_a, Uinds_I, Linds_I, Uinds_A, Linds_A, Index,
fromList2U_a, fromList2L_a, fromList2U_I, fromList2L_I, fromList2U_A, fromList2L_A,
toListfromU_a, toListfromL_a, toListfromU_I, toListfromL_I, toListfromU_A, toListfromL_A,
indexList, swoop, swoopBlocks, interchangeInds, interchangeBlockInds, removeElem, insertElem, replaceElem, 
contractionInd, contractionList_A, contractionList_I, contractionList_a, contractionIndex_A, contractionIndex_I,
contractionIndex_a, insertIndex
) where 


import Data.List
import qualified Data.Map.Strict as Map
import Data.Tuple
import System.IO

--spacetimeindeices take values from 0 to 3

data Uind_a = U0_a | U1_a | U2_a | U3_a deriving (Enum, Eq, Ord, Show)

data Lind_a = L0_a | L1_a | L2_a | L3_a deriving (Enum, Eq, Ord, Show)

--symmetric derivative indices have values from 0 to 9

data Uind_I = U0_I | U1_I | U2_I | U3_I | U4_I | U5_I | U6_I | U7_I | U8_I | U9_I deriving (Enum, Eq, Ord, Show)

data Lind_I = L0_I | L1_I | L2_I | L3_I | L4_I | L5_I | L6_I | L7_I | L8_I | L9_I deriving (Enum, Eq, Ord, Show)

--AreaMetric DOF indices have values form 0 to 20

data Uind_A = U0_A | U1_A | U2_A | U3_A | U4_A | U5_A | U6_A | U7_A | U8_A | U9_A | U10_A | U11_a | U12_a | U13_a | U14_a | U15_a | U16_a | U17_a | U18_a | U19_a | U20_a deriving (Enum, Eq, Ord, Show)

data Lind_A = L0_A | L1_A | L2_A | L3_A | L4_A | L5_A | L6_A | L7_A | L8_A | L9_A | L10_A | L11_a | L12_a | L13_a | L14_a | L15_a | L16_a | L17_a | L18_a | L19_a | L20_a deriving (Enum, Eq, Ord, Show)

--when we export this module we hide the construtors of the indices

--construction is then only possible with toEnum :: (Enum a) => Int -> a 

--on the other hand we can get the specific value with fromEnum :: (Enum a) => a -> Int


--the next step is defining indices as lists of the various ind number thta we constructed

type Uinds_a = [Uind_a]

type Linds_a = [Lind_a]

type Uinds_I = [Uind_I]

type Linds_I = [Lind_I]

type Uinds_A = [Uind_A]

type Linds_A = [Lind_A]

--the indices should be constructed from lists

fromList2U_a :: [Int] -> Uinds_a
fromList2U_a inds = map (\x -> (toEnum x :: Uind_a)) inds 

fromList2L_a :: [Int] -> Linds_a
fromList2L_a inds = map (\x -> (toEnum x :: Lind_a)) inds 

fromList2U_I :: [Int] -> Uinds_I
fromList2U_I inds = map (\x -> (toEnum x :: Uind_I)) inds 


fromList2L_I :: [Int] -> Linds_I
fromList2L_I inds = map (\x -> (toEnum x :: Lind_I)) inds 


fromList2U_A :: [Int] -> Uinds_A
fromList2U_A inds = map (\x -> (toEnum x :: Uind_A)) inds


fromList2L_A :: [Int] -> Linds_A
fromList2L_A inds = map (\x -> (toEnum x :: Lind_A)) inds 

--and we can convert an inds list back to Int list form

toListfromU_a :: Uinds_a -> [Int]
toListfromU_a inds = map fromEnum inds 

toListfromL_a :: Linds_a -> [Int]
toListfromL_a inds = map fromEnum inds 

toListfromU_I :: Uinds_I -> [Int]
toListfromU_I inds = map fromEnum inds 

toListfromL_I :: Linds_I -> [Int]
toListfromL_I inds = map fromEnum inds 

toListfromU_A :: Uinds_A -> [Int]
toListfromU_A inds = map fromEnum inds 

toListfromL_A :: Linds_A -> [Int]
toListfromL_A inds = map fromEnum inds 


--the last step is to write functions for symmetrizing inds

--the next function changes the elements at position (Int,Int) in [a]
        
swoop :: (Ord a,Eq a) => (Int,Int) -> [a] -> [a]
swoop y x
    | max a b >= length x = error "wrong index number to change"
    | a==b = x
    | b<=a = swoop (swap y) x
    | b==(length x)-1 = take a x ++ (x !! b) : init (drop (a+1) x) ++ [(x !! a)] 
    | otherwise = swoop y (fst x1) ++ (snd x1)
      where  x1 = splitAt (b+1) x
             a = fst y
             b = snd y 

--and likewise for blocks

swoopBlocks :: (Ord a, Eq a) => ([Int],[Int]) -> [a] -> [a]
swoopBlocks y x
    | (length a) /= (length b) = error "wrong Block Symmetries"
    | otherwise = foldr swoop x pairList  
          where  a = fst y
                 b = snd y
                 pairList = zip a b
                 
--now define a new datatype that contains the collection of the 6 indlists

type Index = (Uinds_A,Linds_A,Uinds_I,Linds_I,Uinds_a,Linds_a)

--construct it from list

indexList :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> Index
indexList a b c d e f = (fromList2U_A a, fromList2L_A b, fromList2U_I c, fromList2L_I d, fromList2U_a e, fromList2L_a f)

--we also need functions for symmetrizing the indices

interchangeInds :: Int -> (Int,Int) -> Index -> Index
interchangeInds i j (a,b,c,d,e,f) 
        | i == 1 = (swoop j a,b,c,d,e,f)
        | i == 2 = (a,swoop j b,c,d,e,f)
        | i == 3 = (a,b,swoop j c,d,e,f)
        | i == 4 = (a,b,c,swoop j d,e,f)
        | i == 5 = (a,b,c,d,swoop j e,f)
        | i == 6 = (a,b,c,d,e,swoop j f)
        | otherwise = error "wrong index position to interchange"

interchangeBlockInds :: Int -> ([Int],[Int]) -> Index -> Index
interchangeBlockInds i j (a,b,c,d,e,f) 
        | i == 1 = (swoopBlocks j a,b,c,d,e,f)
        | i == 2 = (a,swoopBlocks j b,c,d,e,f)
        | i == 3 = (a,b,swoopBlocks j c,d,e,f)
        | i == 4 = (a,b,c,swoopBlocks j d,e,f)
        | i == 5 = (a,b,c,d,swoopBlocks j e,f)
        | i == 6 = (a,b,c,d,e,swoopBlocks j f)
        | otherwise = error "wrong index positions to interchange"

--we need the functions for the contraction of indices


removeElem :: (Eq a) => Int -> [a] -> [a]
removeElem i b
    | b == [] = error "cannot remove elements from empty list"
    | i >= lb = error "list has not enough elements"
    | i==0 = tail b
    | i == lb = init b
    | otherwise = (fst x) ++ (tail (snd x))
            where x = splitAt i b
                  lb = length b

--there is a problem !!! -> Problem solved (in TensorContraction)

insertElem :: (Eq a) => Int -> a -> [a] -> [a]
insertElem i ins b
    | i > lb || i < 0 = error ("insert position out of index range " ++ "index pos =" ++ (show i) ++ "length = " ++ (show lb))
    | i == lb = b ++ [ins] 
    | otherwise = (fst sb) ++ ins : (snd sb) 
            where 
                    sb = splitAt i b
                    lb = length b
 
replaceElem :: (Eq a) => Int -> a -> [a] -> [a]
replaceElem i ins b
    | b == [] = error "list is empty"
    | i > lb || i < 0 = error "replace position out of index range "
    | i == (lb-1) = init b ++ [ins] 
    | otherwise = (fst sb) ++ ins : (tail (snd sb))
            where 
                    sb = splitAt i b
                    lb = length b


contractionInd :: (Eq a) => (Int,Int) -> ([a],[a]) -> ([a],[a])
contractionInd (i,j) (a,b) = (removeElem i a, removeElem j b)

contractionList_A :: (Int,Int) -> (Uinds_A,Linds_A) -> [(Uinds_A,Linds_A)]
contractionList_A (i,j) (a,b) = zip c d
                where 
                        c = map (\x1 -> insertElem i x1 a) (fromList2U_A [0..20])
                        d = map (\x2 -> insertElem j x2 b) (fromList2L_A [0..20])

contractionList_I :: (Int,Int) -> (Uinds_I,Linds_I) -> [(Uinds_I,Linds_I)]
contractionList_I (i,j) (a,b) = zip c d
                where 
                        c = map (\x1 -> insertElem i x1 a) (fromList2U_I [0..10])
                        d = map (\x2 -> insertElem j x2 b) (fromList2L_I [0..10])
        
contractionList_a :: (Int,Int) -> (Uinds_a,Linds_a) -> [(Uinds_a,Linds_a)]
contractionList_a (i,j) (a,b) = zip c d
                where 
                        c = map (\x1 -> insertElem i x1 a) (fromList2U_a [0..3])
                        d = map (\x2 -> insertElem j x2 b) (fromList2L_a [0..3])

contractionIndex_A :: (Int,Int) -> Index -> [Index]
contractionIndex_A k (a,b,c,d,e,f) = [(fst x, snd x,c,d,e,f) | x <- contractionList_A k (a,b)]

contractionIndex_I :: (Int,Int) -> Index -> [Index]
contractionIndex_I k (a,b,c,d,e,f) = [(a,b,fst x, snd x,e,f) | x <- contractionList_I k (c,d)]

contractionIndex_a :: (Int,Int) -> Index -> [Index]
contractionIndex_a k (a,b,c,d,e,f) = [(a,b,c,d,fst x, snd x) | x <- contractionList_a k (e,f)]

contractionRemovedIndex_A :: (Int,Int) -> Index -> Index
contractionRemovedIndex_A (i,j) (a,b,c,d,e,f) = (removeElem i a, removeElem j b,c,d,e,f)

contractionRemovedIndex_I :: (Int,Int) -> Index -> Index
contractionRemovedIndex_I (i,j) (a,b,c,d,e,f) = (a,b,removeElem i c, removeElem j d,e,f)

contractionRemovedIndex_a :: (Int,Int) -> Index -> Index
contractionRemovedIndex_a (i,j) (a,b,c,d,e,f) = (a,b,c,d,removeElem i e, removeElem j f)

insertIndex :: Int -> Int -> Int -> Index -> Index
insertIndex i j k (a,b,c,d,e,f)
                | i <= 0 || i > 6 = error "wrong index position"
                | i == 1 = (insertElem j (toEnum k :: Uind_A) a,b,c,d,e,f)
                | i == 2 = (a,insertElem j (toEnum k :: Lind_A) b,c,d,e,f)
                | i == 3 = (a,b,insertElem j (toEnum k :: Uind_I) c,d,e,f)
                | i == 4 = (a,b,c,insertElem j (toEnum k :: Lind_I) d,e,f)
                | i == 5 = (a,b,c,d,insertElem j (toEnum k :: Uind_a) e,f)
                | i == 6 = (a,b,c,d,e,insertElem j (toEnum k :: Lind_a) f)