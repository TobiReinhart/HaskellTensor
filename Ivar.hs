--the idea is that it is not enough to store the approp. random values in the pde,
--but for prolongation we need to store (already in teh tensors) not numbers but linear comb. of ivars

--we can encode such a linear combination as a vector with # ivar slots 

--all we need to do is then defining a constructror, a plus function for the vectors, a s-multiplication,
--and a derivative function for the vectors



module Ivar (
Ivar, getIvarScalar, getIvarVec, getIvarLength, mkIvar, zeroIvar, addListComps, addIvar, sMultIvar, subIvar, mkAllIvarsList,
mkAllIvars, number2Ivar, ivar2Number 
    ) where
    
    import Data.List
    import System.IO 
    


    --specify an Ivar Vector by its entry list and its length (for the safe constructor)

    --Ivar contains a number (constants) a vector of ivars and its length
    
    data Ivar a = Ivar a [a] Int deriving (Eq, Ord)

    instance (Show a, Num a, Eq a) => Show (Ivar a) where
        show (Ivar num l i) =  (show num) ++ "*" ++ "V" ++ ( show (ivar2Number (Ivar num l i)) )

            
            
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

    --construct all ivars of given length

    mkAllIvarsList :: (Num a) => Int -> [[a]]
    mkAllIvarsList 1 = [[1]]
    mkAllIvarsList i =  l ++ [1 : (replicate (i-1) 0)]
            where l = zipWith (:) (repeat 0) (mkAllIvarsList (i-1))
            
    mkAllIvars :: (Num a) => Int -> [Ivar a]
    mkAllIvars i =  map (\x -> mkIvar 0 x i) (mkAllIvarsList i) 
    

    --indexing starts at 0 (careful where this might cause additional problems)

    number2Ivar :: (Num a) => Int -> Ivar a
    number2Ivar i = mkIvar 1 l 315
                where 
                    l = (replicate (315-(i+1)) 0) ++ ( 1 : (replicate ((i+1)-1) 0))

    ivar2Number :: (Eq a, Num a) => Ivar a -> Int
    ivar2Number (Ivar num l i) = i - (length zeros)
                 where zeros = takeWhile (\x -> x == 0) l 

