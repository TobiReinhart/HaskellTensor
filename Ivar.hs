--the idea is that it is not enough to store the approp. random values in the pde,
--but for prolongation we need to store (already in teh tensors) not numbers but linear comb. of ivars

--we can encode such a linear combination as a vector with # ivar slots 

--all we need to do is then defining a constructror, a plus function for the vectors, a s-multiplication,
--and a derivative function for the vectors



module Ivar (
Ivar, getIvarScalar, getIvarVec, getIvarLength, mkIvar, zeroIvar, addListComps, addIvar, sMultIvar, subIvar
    ) where
    
    import Data.List
    import System.IO 
    


    --specify an Ivar Vector by its entry list and its length (for the safe constructor)

    --Ivar contains a number (constants) a vector of ivars and its length
    
    data Ivar a = Ivar a [a] Int deriving (Show, Eq, Ord)

    --as we do not want to export the constructor we need functions that return the approp. values stored by Ivar

    getIvarScalar :: Ivar a -> a 
    getIvarScalar (Ivar s l i) = s

    getIvarVec :: Ivar a -> [a]
    getIvarVec (Ivar s l i) = l

    getIvarLength :: Ivar a -> Int
    getIvarLength (Ivar s l i ) = i
    
    mkIvar :: (Num a) => a -> [a] -> Int -> Ivar a
    mkIvar num vec i 
            | length vec /= i = error "entry list does not fir the specified length"
            | otherwise = Ivar num vec i

    zeroIvar :: (Num a) => Int -> Ivar a
    zeroIvar i = Ivar 0 (replicate i 0) i 

    addListComps :: (Num a) => [a] -> [a] -> [a]
    addListComps [] [] = []
    addListComps [x] [y] = [x+y]
    addListComps (x:xs) (y:ys) = (x+y) : addListComps xs ys

    addIvar :: (Num a) => Ivar a -> Ivar a -> Ivar a
    addIvar (Ivar num1 l1 i1) (Ivar num2 l2 i2) 
            | i1 /= i2 = error "Ivars must be of same length to be able ot add"
            | otherwise = Ivar  (num1+num2) (addListComps l1 l2) i1

    sMultIvar :: (Num a) => a -> Ivar a -> Ivar a
    sMultIvar x (Ivar num l i) = Ivar (x*num) (map ((*) x) l) i

    --the only thing missing is a derivative function

    --start with lists

    --actually we do not need subIvar

    subIvar :: (Num a) => Ivar a -> Ivar a -> Ivar a
    subIvar i j = addIvar i (sMultIvar (-1) j) 

    --for first order derivatives only

    