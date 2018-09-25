--this is the module for dealing with pdes

module Pde (
    MultiIndex, diffOrder, diffOrderMult, lengthMult, mkMultiIndex, multIndgetList, cartProdList, mkAllMultiIndsList, mkAllMultiInds,
    mkAllMultiIndsUpto, mkAllMultiIndsUptoRev, addLists, addMultiInds, getValue, Pde, isRightMultInd, removeZeros, mkPde, prolongPdeConstCoeff, combinePdes,
    combinePdesWith, multIndNumberMap, multIndex1toNumber, numbertoMultIndex1, multIndex2toNumber, multIndex2NumberMaple,  isDerivable1, deriveIvar, prolongPdeIvar,
    printtoMaple, printSystoMaple, readPdeTxt, readPde, splitPdeTxt, splitPde, splitPdeSysTxt, splitPdeSys, readPdeSys
    ) where

    import Data.List
    import System.IO
    import Index
    import Tensor
    import Numeric.Natural
    import qualified Data.Map.Strict as Map
    import Data.Maybe
    import Ivar

    --main idea is pde = Map from (Multindex,dvar) to number

    --we need to define a data type for multiindices of all given orders

    --construct a multiindex from length ord and a list

    data MultiIndex = UnsafeMultInd Int Natural [Natural] deriving (Ord,Eq)

    instance Show MultiIndex where
        show (UnsafeMultInd i n l)
                | n == 0 = "const"
                | otherwise = show l

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

    mkAllMultiIndsUptoRev :: Int -> Natural -> [MultiIndex]
    mkAllMultiIndsUptoRev i n
            | n == 0 = mkAllMultiInds i n
            | otherwise =  (mkAllMultiInds i n) ++ (mkAllMultiIndsUptoRev i (n-1))
    

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

    --we need to be able to show this
    
    data Pde a = Pde Int Int Natural (Map.Map (MultiIndex, Int) a)

    --there is probably the error (keys are also ord 2)

    showKey :: (MultiIndex, Int) -> String
    showKey (i,j) 
                | diffOrderMult i == 0 = "L"++(show j)
                | diffOrderMult i == 1 = "L" ++ (show j) ++ ","  ++ (show (multIndex1toNumber i))
                | diffOrderMult i == 2 = "L" ++ (show j) ++ ","  ++ (show (multIndex2toNumber i))
                | otherwise = error "wotks only up to difforder 2"

    instance (Show a) => Show (Pde a) where
        show (Pde i j n pdemap) = show $ map (\x -> "(" ++ (show (snd x)) ++ ")" ++ "*" ++ (showKey (fst x))) l  
                        where l = Map.assocs pdemap

    --extract the value of a given pde

    --this function is probanly slow


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

    removeZeros :: (Eq b) => b -> [(a,b)] -> [(a,b)]
    removeZeros zeroB l = filter (\x -> ((snd x) /= zeroB)) l

    --must remove the ivar zeros

    --now the safe constructor for Pdes from lists [((MultiIndex,Int),a)]

    mkPde :: (Eq a) => a -> Int -> Int -> Natural -> [((MultiIndex,Int),a)] -> Pde a
    mkPde zeroA dvar ivar ord list = Pde dvar ivar ord (Map.fromList inds)
            where list2 = removeZeros zeroA list
                  inds  = map (\x -> (isRightMultInd dvar ivar ord (fst x), snd x)) list2

    --removeZeros must remove ivar zeros (i.e. this function removes all values that are equal to zeroA)

    --right now this (the following) works only for pdes with constant coefficients

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

    --this function should probably be realized as a Map!!!!

    multIndex1toNumber :: MultiIndex -> Int
    multIndex1toNumber (UnsafeMultInd i ord l)
            |  ord /= 1 = error ("only valid for diffOrder 1, called with" ++ " " ++ (show ord ))
            | otherwise = i - (length zeros)
             where zeros = takeWhile (\x -> x == 0) l 

    multIndex2toNumber :: MultiIndex -> (Int,Int)
    multIndex2toNumber (UnsafeMultInd i ord l)
            |  ord /= 2 = error ("only valid for diffOrder 2, called with" ++ " " ++ (show ord ))
            | otherwise = (i-(lz+lz2),i - lz)
             where 
                zeros1 = takeWhile (\x -> x == 0) l
                lz = length zeros1 
                newlist = ((l !! lz) -1) : (drop (lz+1) l) 
                lz2 = length $ takeWhile (\x -> x==0) newlist

    multIndex2NumberMaple :: MultiIndex -> Int
    multIndex2NumberMaple (UnsafeMultInd i ord l) 
                | ord == 0 = 50086
                | ord == 1 = 49770 + (multIndex1toNumber (UnsafeMultInd i ord l))
                | ord == 2 = ((fst p)-1)*315 + (snd p) 
                | otherwise = error "works only for multiinds up to order 2"
                 where p = multIndex2toNumber (UnsafeMultInd i ord l)


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
            | l !! (j - (multIndex1toNumber mult)) /= 0 = True
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
                 entry = (l !! (j-(multIndex1toNumber mult)))

    --the function a -> a is the derivative function for the elements

    prolongPdeIvar :: (Num a,Ord a) => MultiIndex  -> Pde (Ivar a) -> Pde (Ivar a)
    prolongPdeIvar mult (Pde i j n map1) = combinePdesWith f (prolongPdeConstCoeff mult (Pde i j n map1)) pde2
        where 
            f = addIvar
            map2 =  Map.map (deriveIvar mult) (Map.filter (isDerivable1 mult) map1) 
            pde2 = Pde i j n map2


    --the only part that is still unclear is how to convert the result of the tensor evaluation to a pde ??

    --there is still some error in prolongPdeIvar (at some point a multiindex of rank 2 is constructed)

    --this wotks only for the pdes with only 1 dvar!! and only for 1 pde

    --store the matrixrow (i.e. the eqn number) as argument in print to maple

    printtoMaple :: (Num a, Ord a, Show a) => (Pde (Ivar a),Int) -> String
    printtoMaple ((Pde i j n mapPde),eqnNr) = ( foldl (++) " " $ map (\x -> (show $ (eqnNr,multIndex2NumberMaple (fst (fst x)))) ++ "=" ++  (show $ snd x) ++ "," ) list) 
                where 
                        list = Map.assocs mapPde

    printSystoMaple :: (Num a, Ord a, Show a) => [Pde (Ivar a)] -> String
    printSystoMaple  sys = "{" ++ (foldl (\a b -> a ++ (printtoMaple b)) " " l) ++ "}" 
                        where l = zip sys [1..]


    --this function reads a pde in list format (explicit list of equations) from text to a pde

    readPdeTxt :: [String] -> [(Ivar Double)]
    readPdeTxt s = map readIvar s

    splitPdeTxt :: String -> [String]
    splitPdeTxt [] = []
    splitPdeTxt s = s1 : splitPdeTxt s2
                        where
                                s1 = takeWhile (/= ',') s
                                s2 = safeTail $  dropWhile (/= ',') s

    splitPde :: String -> [String]
    splitPde = splitPdeTxt.init.tail 

    readPde :: String -> [(Ivar Double)]
    readPde = readPdeTxt.splitPde

    splitPdeSysTxt :: String -> [String]
    splitPdeSysTxt [] = []
    splitPdeSysTxt s = ('[' : s1 ++ [']']) : ( splitPdeSysTxt s2)
                        where
                                s1 = takeWhile (/= ']') $ tail s
                                s2 = dropWhile (/= '[') $ tail s 

    splitPdeSys :: String -> [String]
    splitPdeSys = splitPdeSysTxt.init.tail


    readPdeSys :: String -> [[(Ivar Double)]]
    readPdeSys s = map readPde (splitPdeSys s)

  