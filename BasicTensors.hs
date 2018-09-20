module BasicTensors (
    symIndList, symMult2_a, symMultMap2_a, isAreaOrdered, areaDofList, areaMult, areaMultMap, deltaF_a, delta_a,
    deltaF_I, delta_I, deltaF_A, delta_A, interMap_I, interF_I, inter_I, interF_J, inter_J,
    interMap_A, canonicalizeArea, inter_A, interF_B, inter_B, interMetric, interArea

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

    --map of symmetric index tuples and corresponding multiplicity

    symMult2_a :: (Num a) => [Int] -> a
    symMult2_a [a,b] 
        | a == b = 1
        | otherwise = 2
    symMult2_a l = error "wrong number of indices" 

    symMultMap2_a :: Map.Map [Int] Int
    symMultMap2_a = let a = symIndList 2 3 in Map.fromList $ zip a $ map symMult2_a a  

    --now the same for the are metric

    isAreaOrdered :: (Num a, Ord a) => [a] -> Bool
    isAreaOrdered [a,b,c,d] 
            | a < b && a < c && c < d = True
            | a < b && a == c && c < d && b<=d = True
            | otherwise = False
    isAreaOrdered l
            | length l /= 4 = error "wrong list length"
            | otherwise = error "shoul be matched with first block" 

    --list of 21 area metric dofs

    areaDofList :: [[Int]]
    areaDofList = [ [a,b,c,d] | a <- [0..3], b <- [0..3], c <- [0..3], d <- [0..3], isAreaOrdered([a,b,c,d]) ]


    --corresponding multiplicities

    areaMult :: (Num a) => [Int] -> a
    areaMult [a,b,c,d] 
        | a == c && b == d = 4
        | otherwise = 8
    areaMult l = error "wrong number of indices"

    areaMultMap :: Map.Map [Int] Int
    areaMultMap = let a = areaDofList in  Map.fromList $ zip a $ map areaMult a

    --defien a delta for all 3 possible indices

    deltaF_a :: (Num a) => Index -> a
    deltaF_a ([],[],[],[],a,b) 
        | length a == 1 && length b == 1 && toListfromU_a a == toListfromL_a b = 1
        |  length a == 1 && length b == 1 = 0
        | otherwise = error "wrong number of indices"
    deltaF_a ind = error "delta has only 2 indices"

    delta_a :: (Num a) => Tensor a
    delta_a = Tensor (0,0,0,0,1,1) deltaF_a

    deltaF_I :: (Num a) => Index -> a
    deltaF_I ([],[],i,j,[],[]) 
        | length i == 1 && length j == 1 && toListfromU_I i == toListfromL_I j = 1
        | length i == 1 && length j == 1 = 0
        | otherwise = error "wrong number of indices"
    deltaF_I ind = error "delta has only 2 indices"

    delta_I :: (Num a) => Tensor a
    delta_I = Tensor (0,0,1,1,0,0) deltaF_I

    deltaF_A :: (Num a) => Index -> a
    deltaF_A (i,j,[],[],[],[]) 
        | length i == 1 && length j == 1 && toListfromU_A i == toListfromL_A j = 1
        | length i == 1 && length j == 1 = 0
        | otherwise = error "wrong number of indices"
    deltaF_A ind = error "delta has only 2 indices"

    delta_A :: (Num a) => Tensor a
    delta_A = Tensor (1,1,0,0,0,0) deltaF_A

    --define the symmetric 2-index intertwiner (we need the function)

    interMap_I :: Map.Map [Int] [Int]
    interMap_I = Map.fromList $ zipWith (\x y -> ([x],y)) [0..9] $ symIndList 2 3

    interF_I :: (Num a) => Index -> a 
    interF_I ([],[],[i],[],[],[j,k])
            | elem (sort (toListfromL_a [j,k])) (Map.lookup (toListfromU_I [i]) map) = 1
            | otherwise = 0
             where map = interMap_I   
    interF_I ind = error "wrong number of indices"
    
    inter_I :: (Num a) => Tensor a
    inter_I = Tensor (0,0,1,0,0,2) interF_I

    --and the "inverse" intertwiner

    --divide or mult by factor???

    interF_J :: (Fractional a) => Index -> a 
    interF_J ([],[],[],[i],[j,k],[])
            | elem  s (Map.lookup (toListfromL_I [i]) map1) = (1/symMult2_a s) 
            | otherwise = 0
             where 
                s = sort (toListfromU_a [j,k])
                map1 = interMap_I 
    interF_J ind = error "wrong number of indices"
    
    inter_J :: (Fractional a) => Tensor a
    inter_J = Tensor (0,0,1,0,0,2) interF_J

    --now the same for the area metric intertwiner

    --we need a map that maps between Area A inds and the associated 4 spacetime inds

    interMap_A :: Map.Map [Int] [Int]
    interMap_A = Map.fromList $ zipWith (\x y -> ([x],y)) [0..20] $ areaDofList

    --we need a function that canonically sorts the area metric indices and computes the corresponding sign

    --does not test for index ranges

    canonicalizeArea :: (Num a) => (a,[Int]) -> (a,[Int])
    canonicalizeArea (i,[a,b,c,d]) 
                | isAreaOrdered [a,b,c,d] = (i,[a,b,c,d])
                | c < a || (c == a && b > d) = canonicalizeArea (i,[c,d,a,b])
                | a > b = canonicalizeArea (-i,[b,a,c,d])
                | c > d = canonicalizeArea (-i,[a,b,d,c])
                | otherwise = (0,[a,b,c,d])
    canonicalizeArea (i,j) = error ("wrong index number" ++ " " ++ (show (j)))

    --define the intertwiner with area index up

    interF_A :: (Num a) => Index -> a
    interF_A ([i],[],[],[],[],[a,b,c,d])
                | elem ( snd canonical ) (Map.lookup (toListfromU_A [i]) map) = fst canonical
                | otherwise = 0
                 where 
                    canonical = canonicalizeArea (1,toListfromL_a [a,b,c,d])
                    map = interMap_A
    interF_A j = error "wrong Index"

    --define the corresponding Tensor
    inter_A :: (Num a) => Tensor a
    inter_A = Tensor (1,0,0,0,0,4) interF_A

    --now the intertwiner with the area index in the low position

    --divide or mult by factor???

    interF_B :: (Fractional a) => Index -> a
    interF_B ([],[i],[],[],[a,b,c,d],[])
                | elem ( snd canonical ) (Map.lookup (toListfromL_A [i]) map) = (fst canonical) * (1/ (areaMult ( snd canonical )))
                | otherwise = 0
                 where 
                    canonical = canonicalizeArea (1,toListfromU_a [a,b,c,d])
                    map = interMap_A
    interF_B j = error "wrong Index"

    --define the Tensor

    inter_B :: (Fractional a) => Tensor a
    inter_B = Tensor (0,1,0,0,4,0) interF_B

    --now define the GOTAY MARSDEN intertwiner of the Metric and Area Metric as a contraction of the corresp. intertwiners

    interMetric :: (Fractional a) => Tensor a
    interMetric = tensorSMult (-2) $ tensorContract_a (0,0) prod 
            where 
                i = inter_I
                j = inter_J
                prod = tensorProduct i j  

    
    interArea :: (Fractional a) => Tensor a
    interArea = tensorSMult (-4) $ tensorContract [] [] [(1,1),(2,2),(3,3)] prod 
                where
                    i = inter_A
                    j = inter_B
                    prod = tensorProduct i j
                    