--the idea is that it is not enough to store the approp. random values in the pde,
--but for prolongation we need to store (already in teh tensors) not numbers but linear comb. of ivars

--we can encode such a linear combination as a vector with # ivar slots 

--all we need to do is then defining a constructror, a plus function for the vectors, a s-multiplication,
--and a derivative function for the vectors



module Ivar (


    ) where
    
    import Data.List
    import System.IO 
    import Pde


    --specify an Ivar Vector by its entry list and its length (for the safe constructor)
    
    data Ivar a = Ivar [a] Int deriving (Show, Eq, Ord)
    
    mkIvar :: (Num a) => [a] -> Int -> Ivar a
    mkIvar vec i 
            | length vec /= i = error "entry list does not fir the specified length"
            | otherwise = Ivar vec i

    addListComps :: (Num a) => [a] -> [a] -> [a]
    addListComps [] [] = []
    addListComps [x] [y] = [x+y]
    addListComps (x:xs) (y:ys) = (x+y) : addListComps xs ys

    addIvar :: (Num a) => Ivar a -> Ivar a -> Ivar a
    addIvar (Ivar l1 i1) (Ivar l2 i2) 
            | i1 /= i2 = error "Ivars must be of same length to be able ot add"
            | otherwise = Ivar (addListComps l1 l2) i1

    sMultIvar :: (Num a) => a -> Ivar a -> Ivar a
    sMultIvar x (Ivar l i) = Ivar (map ((*) x) l) i

    --the only thing missing is a derivative function

    --start with lists

    --actually we do not need subIvar

    subIvar :: (Num a) => Ivar a -> Ivar a -> Ivar a
    subIvar i j = addIvar i (sMultIvar (-1) j) 

    --for first order derivatives only

    --check if the derivative yields zero (otherwise it is just given by subtraction of inds)

    isDerivable1 :: (Num a, Eq a, Ord a) => MultiIndex -> Ivar a -> Bool
    isDerivable1 mult (Ivar l2 j) 
            | i /= j = error "derivative mult ind must be length as ivar vec"
            | l2 !! (multIndex1toNumber mult) /= 0 = True
            | otherwise = False
             where 
                i = lengthMult mult
