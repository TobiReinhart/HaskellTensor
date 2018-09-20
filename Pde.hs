--this is the module for dealing with pdes

module Pde (

    ) where

    import Data.List
    import System.IO
    import Index
    import Tensor
    import BasicTensors
    import Numeric.Natural
    import qualified Data.Map.Strict as Map

    --main idea is pde = Map from (Multindex,dvar) to number

    --we need to define a data type for multiindices of all given orders

    --construct a multiindex from length ord and a list

    data MultiIndex = UnsafeMultInd Int Natural [Natural] deriving (Show,Ord,Eq)

    --write a safe constructor

    diffOrder :: [Natural] -> Natural 
    diffOrder i = foldl (+) 0 i

    diffOrderMult :: MultiIndex -> Natural
    diffOrderMult (UnsafeMultInd i ord l) = ord

    lengthMult :: MultiIndex -> Int
    lengthMult (UnsafeMultInd i ord l) = i

    --the safe constructor

    mkMultiIndex :: Int -> Natural -> [Natural] -> MultiIndex
    mkMultiIndex i n list
            | length list == i && diffOrder list == n = UnsafeMultInd i n list
            | otherwise = error "list does not fit multiindex specification"

    --this is too slow

    --we need a faster way to constract all multiindices of a given sizemk

    cartProdList :: [[a]] -> [[a]]
    cartProdList [a] =  [ [x] | x <- a]
    cartProdList [a,b] = [ [x,y] | x <- a, y <- b ]
    cartProdList (x : xs) = [ a : b | a <- x, b <- cartProdList xs]

    --needs to be improved

    mkAllMultiInds :: Int -> Natural -> [MultiIndex]
    mkAllMultiInds i n = [mkMultiIndex i n l | l <- (cartProdList (replicate i [0..n])), diffOrder l == n ]

    mkAllMultiIndsUpto :: Int -> Natural -> [MultiIndex]
    mkAllMultiIndsUpto i n
            | n == 0 = mkAllMultiInds i n
            | otherwise = (mkAllMultiIndsUpto i (n-1)) ++ (mkAllMultiInds i n)

    --we need to be able to prolong multiindices

    addLists :: (Num a) => [a] -> [a] -> [a]
    addLists [x] [] = addLists [] [x]
    addLists [] [y] = error "lists must have same length"
    addLists [x] [y] = [x+y]
    addLists x y = (head x+ head y) : ( addLists (tail x) (tail y) )

    --this fucntion can be used for prolongation
    
    addMultiInds :: MultiIndex -> MultiIndex -> MultiIndex
    addMultiInds (UnsafeMultInd i1 n1 l1) (UnsafeMultInd i2 n2 l2) = mkMultiIndex i1 (n1+n2) (addLists l1 l2)

    --now define the pde data type (a pde system is a alist of pdes) (with coeffs in of type a

    --pde is constructed by dvar ivar ord and a map from (Multiindex of ord length ivar, dvar) to value a

    --it is only important how many dvars and ivars exist

    --Pde nops dVar nops Ivar Ord Map (MultiInd, #dvar) value
    
    data Pde a = Pde Int Int Natural (Map.Map (MultiIndex, Int) a)

    --extract the value of a given pde

    getValue :: Pde a -> (MultiIndex,Int) -> Maybe a
    getValue (Pde dvar ivar ord  map) (ind,var) 
            | diffOrderMult ind > ord = error "difforder of MultiInd is higher than pde ord"
            | 0 < var && var  < dvar = error "pde is evaluated at dvar that ist higher as number of dvars"
            | lengthMult ind /= ivar = error "MultiIndex has wrong length compared to number of ivars"
            | otherwise = Map.lookup (ind,var) map 

    --we need a safe constructor for pde

    --we can construct it from a list of key value pairs 

    --check if nops dvar fits dvar in key, nops ivar fits length MultInd in key and ord fits diffOrder in key

    isRightMultInd :: Int -> Int -> Natural -> (MultiIndex,Int) -> (MultiIndex,Int)
    isRightMultInd dvar ivar ord (ind, var)
            | diffOrderMult ind > ord = error "difforder of MultiInd is higher than pde ord"
            | 0 < var && var < dvar = error "pde is evaluated at dvar that ist higher as number of dvars"
            | lengthMult ind /= ivar = error "MultiIndex has wrong length compared to number of ivars"
            | otherwise = (ind,var)

    removeZeros :: (Num b,Eq b) => [(a,b)] -> [(a,b)]
    removeZeros l = filter (\x -> ((snd x) /= 0 && (snd x) /= (-0))) l

    --now the safe constructor for Pdes from lists [((MultiIndex,Int),a)]

    mkPde :: (Num a, Eq a) => Int -> Int -> Natural -> [((MultiIndex,Int),a)] -> Pde a
    mkPde dvar ivar ord list = Pde dvar ivar ord (Map.fromList inds)
            where list2 = removeZeros list
                  inds  = map (\x -> (isRightMultInd dvar ivar ord (fst x), snd x)) list2

    
    prolongPde :: MultiIndex -> Pde a -> Pde a
    prolongPde i (Pde dvar ivar ord map) = Pde dvar ivar (ord + diffOrderMult i)  (Map.mapKeys (\x -> (addMultiInds i (fst x), snd x)) map) 


    --we need functions to combine pdes (if dvar and ivar are the same)

    --still only for combining different parts of one pde
    
    combinePdes :: (Num a) => Pde a -> Pde a -> Pde a
    combinePdes (Pde dvar1 ivar1 ord1 map1) (Pde dvar2 ivar2 ord2 map2)
            | dvar1 /= dvar2 = error "cannot combine 2 pdes with different dvars"
            | ivar1 /= ivar2 = error "cannot combine 2 pdes with different ivars"
            |  otherwise = Pde dvar1 ivar1 (ord1 + ord2) (Map.unionWith (+) map1 map2)

    --we need to print the pde in maple form

    
    multIndNumberMap :: Int -> Natural -> (Map.Map MultiIndex Int)
    multIndNumberMap i n = Map.fromList $ zip l [1..length l]
            where l = mkAllMultiIndsUpto i n 

            
    



