--this is the module for dealing with pdes

module Pde (
    MultiIndex, diffOrder, diffOrderMult, lengthMult, mkMultiIndex, multIndgetList, cartProdList, mkAllMultiIndsList, mkAllMultiInds,
    mkAllMultiIndsUpto, addLists, addMultiInds, getValue, Pde, isRightMultInd, removeZeros, mkPde, prolongPdeConstCoeff, combinePdes,
    combinePdesWith, multIndNumberMap, multIndex1toNumber, numbertoMultIndex1, isDerivable1, deriveIvar, prolongPdeIvar
    ) where

    import Data.List
    import System.IO
    import Index
    import Tensor
    import BasicTensors
    import Numeric.Natural
    import qualified Data.Map.Strict as Map
    import Data.Maybe
    import Ivar

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

    multIndgetList :: MultiIndex -> [Natural]
    multIndgetList (UnsafeMultInd i n l) = l 

    --this is too slow

    --we need a faster way to constract all multiindices of a given sizemk

    cartProdList :: [[a]] -> [[a]]
    cartProdList [a] =  [ [x] | x <- a]
    cartProdList [a,b] = [ [x,y] | x <- a, y <- b ]
    cartProdList (x : xs) = [ a : b | a <- x, b <- cartProdList xs]

    --needs to be improved

    mkAllMultiIndsList :: Int -> Natural -> [[Natural]]
    mkAllMultiIndsList 0 _ = []
    mkAllMultiIndsList i 0 =   [(replicate i 0)]
    mkAllMultiIndsList 1 n =  [[n]]
    mkAllMultiIndsList i n = sort $ concat $ zipWith h [0..n] f
            where 
                l = map (\x -> (n - x) ) [0..n]
                f = map (mkAllMultiIndsList (i-1)) l
                h = \ x y -> map (\z -> x : z) y

    mkAllMultiInds :: Int -> Natural -> [MultiIndex]
    mkAllMultiInds i n = map (mkMultiIndex i n) $ mkAllMultiIndsList i n

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

    getPdeValue :: Pde a -> (MultiIndex,Int) -> Maybe a
    getPdeValue (Pde dvar ivar ord  map) (ind,var) 
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

    --right now this works only for pdes with constant coefficients

    --this is not the case for the diffeo equivariance equations as here there are coefficints that are ivars

    prolongPdeConstCoeff :: MultiIndex -> Pde a -> Pde a
    prolongPdeConstCoeff i (Pde dvar ivar ord map) = Pde dvar ivar (ord + diffOrderMult i)  (Map.mapKeys (\x -> (addMultiInds i (fst x), snd x)) map) 

    --combine parts of the same pde
    
    combinePdes :: (Num a) => Pde a -> Pde a -> Pde a
    combinePdes (Pde dvar1 ivar1 ord1 map1) (Pde dvar2 ivar2 ord2 map2)
            | dvar1 /= dvar2 = error "cannot combine 2 pdes with different dvars"
            | ivar1 /= ivar2 = error "cannot combine 2 pdes with different ivars"
            |  otherwise = Pde dvar1 ivar1 (max ord1 ord2) (Map.unionWith (+) map1 map2)


    combinePdesWith :: (a -> a -> a) -> Pde a -> Pde a -> Pde a
    combinePdesWith plus (Pde dvar1 ivar1 ord1 map1) (Pde dvar2 ivar2 ord2 map2)
            | dvar1 /= dvar2 = error "cannot combine 2 pdes with different dvars"
            | ivar1 /= ivar2 = error "cannot combine 2 pdes with different ivars"
            |  otherwise = Pde dvar1 ivar1 (max ord1 ord2) (Map.unionWith plus map1 map2)

        
    --we need to print the pde in maple form

    --carefull how we translate between multiInds and Numbers (consistency)

    multIndNumberMap :: Int -> Natural -> (Map.Map MultiIndex Int)
    multIndNumberMap i n = Map.fromList $ zip l [1..length l]
            where l = mkAllMultiIndsUpto i n 

    multIndex1toNumber :: MultiIndex -> Int
    multIndex1toNumber (UnsafeMultInd i ord l)
            |  ord /= 1 = error "only valid for diffOrder 1"
            | otherwise = i - (length zeros)
             where zeros = takeWhile (\x -> x == 0) l 

    numbertoMultIndex1 :: Int -> Int -> MultiIndex
    numbertoMultIndex1 i j = mkMultiIndex i 1 l
            where l = (replicate (i-j) 0) ++ ( 1 : (replicate (j-1) 0))

            
--the information about the ivars in the pde is encoded in the tensors

--once we extract the pde we have ivars stored in the pde (must be taken care when prolonging)

--we need derivative functions for the ivars

    --check if the derivative yields zero (otherwise it is just given by subtraction of inds)

    isDerivable1 :: (Num a, Eq a, Ord a) => MultiIndex -> Ivar a -> Bool
    isDerivable1 mult var
            | i /= j = error "derivative mult ind must be length as ivar vec"
            | l !! (multIndex1toNumber mult) /= 0 = True
            | otherwise = False
              where 
                num = getIvarScalar var
                l = getIvarVec var
                j = getIvarLength var 
                i = lengthMult mult
  
    deriveIvar :: (Num a,Eq a) => MultiIndex -> Ivar a -> Ivar a
    deriveIvar mult ivar 
            | i /= j = error "derivative mult ind must be length as ivar vec"
            | entry /= 0 = mkIvar entry (replicate j 0) j
            | otherwise = zeroIvar j
              where 
                 num = getIvarScalar ivar
                 l = getIvarVec ivar
                 j = getIvarLength ivar 
                 i = lengthMult mult
                 entry = l !! (multIndex1toNumber mult)

    --the function a -> a is the derivative function for the elements

    prolongPdeIvar :: (Num a,Ord a) => MultiIndex  -> Pde (Ivar a) -> Pde (Ivar a)
    prolongPdeIvar mult (Pde i j n map1) = combinePdesWith f (prolongPdeConstCoeff mult (Pde i j n map1)) pde2
        where 
            f = addIvar
            map2 =  Map.map (deriveIvar mult) (Map.filter (isDerivable1 mult) map1) 
            pde2 = Pde i j n map2


