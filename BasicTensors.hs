module BasicTensors (
    symIndList, symEvalList2, symEvalList3, symMult2_a, symMultMap2_a, isAreaOrdered, areaDofList, areaMult, areaMultMap, deltaF_a, delta_a,
    deltaF_I, delta_I, deltaF_A, delta_A, interMap_I, interF_I, inter_I, interF_J, inter_J,
    interMap_A, canonicalizeArea, inter_A, interF_B, inter_B, interMetric, interArea, ivarTensor1List, 
    ivarTensor2List, ivarTensor3List, ivarTensor1F, ivarTensor2F, ivarTensor3F, 
    ivar1Tensor, ivar2Tensor, ivar3Tensor, sym2_a, sym3_a,
    eqn1_1, eqn1_1Comps, eqn1_2Intertwiner, eqn1_2, eqn1_2Comps, eqn1_3Intertwiner, eqn1_3, eqn1_3Comps, eqn1_4Comps, 
    eqn2_1Comps, eqn2_2, eqn2_2Comps, eqn2_3Inter, eqn2_3, eqn2_3Comps, eqn2_4Comps,
    eqn3_1Comps, eqn3_2Comps, eqn3_3Inter, eqn3_3, eqn3_3Comps, eqn3_4Comps

) where
    
    import Index
    import Tensor
    import Ivar
    import Pde
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

    --the following 2 functions generate the lists that are needed for contractions with symmetric indices

    --the first Int specifies the indexposition second and third the 2 indices in the corresponding slot that are to be evaluated

    symEvalList2 ::  (Int,Int,Int) -> [[(Int,Int,Int)]]
    symEvalList2 (i,j,k)   
            | i == 1 || i == 2 = zipWith (\x y -> [x,y]) (map (\x -> (i,j,x !! 0)) (symIndList 2 20)) (map (\x -> (i,k,x !! 1)) (symIndList 2 20))
            | i == 3 || i == 4 = zipWith (\x y -> [x,y]) (map (\x -> (i,j,x !! 0)) (symIndList 2 9)) (map (\x -> (i,k,x !! 1)) (symIndList 2 9))
            | i == 5 || i == 6 = zipWith (\x y -> [x,y]) (map (\x -> (i,j,x !! 0)) (symIndList 2 3)) (map (\x -> (i,k,x !! 1)) (symIndList 2 3))
            | otherwise = error "wrong indexposition for evaluating tensor over 2 sym inds"

    symEvalList3 ::  (Int,Int,Int,Int) -> [[(Int,Int,Int)]]
    symEvalList3 (i,j,k,l)   
            | i == 1 || i == 2 = zipWith3 (\x y z -> [x,y,z]) (map (\x -> (i,j,x !! 0)) (symIndList 3 20)) (map (\x -> (i,k,x !! 1)) (symIndList 3 20)) (map (\x -> (i,l,x !! 2)) (symIndList 3 20))
            | i == 3 || i == 4 = zipWith3 (\x y z -> [x,y,z]) (map (\x -> (i,j,x !! 0)) (symIndList 3 9)) (map (\x -> (i,k,x !! 1)) (symIndList 3 9)) (map (\x -> (i,l,x !! 2)) (symIndList 3 9))
            | i == 5 || i == 6 = zipWith3 (\x y z -> [x,y,z]) (map (\x -> (i,j,x !! 0)) (symIndList 3 3)) (map (\x -> (i,k,x !! 1)) (symIndList 3 3)) (map (\x -> (i,l,x !! 2)) (symIndList 3 3))
            | otherwise = error "wrong indexposition for evaluating tensor over 2 sym inds"

    --eval tensor over sym inds can now be archieved by mapping evalTensor over symEvalListi 

    --map of symmetric index tuples and corresponding multiplicity

    symMult2_a :: (Num a) => [Int] -> a
    symMult2_a [a,b] 
        | a == b = 1
        | otherwise = 2
    symMult2_a l = error "wrong number of indices" 

    symMultMap2_a :: Map.Map [Int] Int
    symMultMap2_a = let a = symIndList 2 3 in Map.fromList $ zip a $ map symMult2_a a  

    --now the same for the are metric

    --this function is slow!!!!

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

    --define them as doubles 

    deltaF_a :: (Num a) => Index -> a
    deltaF_a ([],[],[],[],a,b) 
        | length a == 1 && length b == 1 && toListfromU_a a == toListfromL_a b = 1
        |  length a == 1 && length b == 1 = 0
        | otherwise = error "wrong number of indices"
    deltaF_a ind = error "delta has only 2 indices"

    delta_a :: Tensor Double
    delta_a = Tensor (0,0,0,0,1,1) deltaF_a

    deltaF_I :: (Num a) => Index -> a
    deltaF_I ([],[],i,j,[],[]) 
        | length i == 1 && length j == 1 && toListfromU_I i == toListfromL_I j = 1
        | length i == 1 && length j == 1 = 0
        | otherwise = error "wrong number of indices"
    deltaF_I ind = error "delta has only 2 indices"

    delta_I :: Tensor Double
    delta_I = Tensor (0,0,1,1,0,0) deltaF_I

    deltaF_A :: (Num a) => Index -> a
    deltaF_A (i,j,[],[],[],[]) 
        | length i == 1 && length j == 1 && toListfromU_A i == toListfromL_A j = 1
        | length i == 1 && length j == 1 = 0
        | otherwise = error "wrong number of indices"
    deltaF_A ind = error "delta has only 2 indices"

    delta_A :: Tensor Double
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
    
    inter_I :: Tensor Double
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
    
    inter_J :: Tensor Double
    inter_J = Tensor (0,0,0,1,2,0) interF_J

    --now the same for the area metric intertwiner

    --we need a map that maps between Area A inds and the associated 4 spacetime inds

    interMap_A :: Map.Map [Int] [Int]
    interMap_A = Map.fromList $ zipWith (\x y -> ([x],y)) [0..20] $ areaDofList

    --we need a function that canonically sorts the area metric indices and computes the corresponding sign

    --does not test for index ranges

    --this is slow!!!!!! (there are some safety checks in isAreaOrdered)

    --there is the problem ?!

    --rename this to version2 and use the new implementation

    canonicalizeArea2 :: (Num a) => (a,[Int]) -> (a,[Int])
    canonicalizeArea2 (i,[a,b,c,d]) 
                | isAreaOrdered [a,b,c,d] = (i,[a,b,c,d])
                | c < a || (c == a && b > d) = canonicalizeArea (i,[c,d,a,b])
                | a > b = canonicalizeArea (-i,[b,a,c,d])
                | c > d = canonicalizeArea (-i,[a,b,d,c])
                | otherwise = (0,[a,b,c,d])
    canonicalizeArea2 (i,j) = error ("wrong index number" ++ " " ++ (show (j)))

    --instead implement the area order by sorting appropriate sublists of the ind list

    aSort2 :: (Ord a, Num b) => (b,[a]) -> (b,[a])
    aSort2 (i,[x,y]) 
                | x <= y = (i,[x,y])
                | otherwise = (-i,[y,x])
    aSort2 l = error "wrong number of elements in the list"

    --blocks can be sorted by sort

    canonicalizeArea :: (Num a) => (a,[Int]) -> (a,[Int])
    canonicalizeArea (i,[a,b,c,d])
                | a == b || c == d = (0,[a,b,c,d])
                | otherwise = (fst sndBlock, concat res)
                where 
                    fstBlock = aSort2 (i,[a,b])
                    sndBlock = aSort2 (fst fstBlock, [c,d])
                    res = sort [snd fstBlock, snd sndBlock]
    canonicalizeArea l = error "wromg number of elems in are ind list"




    --define the intertwiner with area index up

    --maybe it is faster to define the functions interF_A and interF_B without ordering the indices ??

    interF_A :: (Num a) => Index -> a
    interF_A ([i],[],[],[],[],[a,b,c,d])
                | elem ( snd canonical ) (Map.lookup (toListfromU_A [i]) map) = fst canonical
                | otherwise = 0
                 where 
                    canonical = canonicalizeArea (1,toListfromL_a [a,b,c,d])
                    map = interMap_A
    interF_A j = error "wrong Index"

    --define the corresponding Tensor
    inter_A :: Tensor Double
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

    inter_B :: Tensor Double
    inter_B = Tensor (0,1,0,0,4,0) interF_B

    --now define the GOTAY MARSDEN intertwiner of the Metric and Area Metric as a contraction of the corresp. intertwiners

    interMetric :: Tensor Double
    interMetric = tensorSMult (-2) $ tensorContract_a (0,0) prod 
            where 
                i = inter_I
                j = inter_J
                prod = tensorProduct i j  

    
    interArea :: Tensor Double
    interArea = tensorSMult (-4) $ tensorContract [] [] [(1,1),(2,2),(3,3)] prod 
                where
                    i = inter_A
                    j = inter_B
                    prod = tensorProduct i j

    --we need 3 more tensors that store the three different rnages of values of ivars

    ivarTensor1List :: (Num a) => [Ivar a]
    ivarTensor1List =  take 21 $ mkAllIvars 315

    ivarTensor2List :: (Num a) => [Ivar a]
    ivarTensor2List = take (4*21) $ drop 21 $ mkAllIvars 315

    ivarTensor3List :: (Num a) => [Ivar a]
    ivarTensor3List = take (10*21) $ drop (5*21) $ mkAllIvars 315

    --now construct the corresponding tensors

    --we need the functions for constructing the tensor (number2Ivar in Ivar)

    --now we can construct the 3 functions
    
    ivarTensor1F :: (Num a) => Index -> Ivar a
    ivarTensor1F  ([],[a],[],[],[],[]) =  number2Ivar (fromEnum a +1)  
    ivarTensor1F l = error "function ivarTensor1F is evaluated at wrong index"

    --we eval the geomatry index in the outer loop and the spacetime index in the inner loop

    ivarTensor2F :: (Num a) => Index -> Ivar a
    ivarTensor2F ([],[a],[],[],[],[b]) = number2Ivar $ (fromEnum a)*4 + (fromEnum b+1) + 21
    ivarTensor2F j = error "function ivarTensor2F evaluated at wrong index"

    ivarTensor3F :: (Num a) => Index -> Ivar a
    ivarTensor3F ([],[a],[],[b],[],[]) = number2Ivar $ (fromEnum a)*10 + (fromEnum b+1) + 105
    ivarTensor3F j = error "function ivarTensor3F evaluated at wrong index"

    --now defien the Tensors

    ivar1Tensor :: Tensor (Ivar Double)
    ivar1Tensor = Tensor (0,1,0,0,0,0) ivarTensor1F

    ivar2Tensor :: Tensor (Ivar Double)
    ivar2Tensor = Tensor (0,1,0,0,0,1) ivarTensor2F

    ivar3Tensor :: Tensor (Ivar Double)
    ivar3Tensor = Tensor (0,1,0,1,0,0) ivarTensor3F

    --we also need symmetrizer for tensors with upper indices (an dfor the moment only for spacetimeindices)

    sym2_a :: Tensor Double
    sym2_a = symmetrizeTensor 5 (0,1) $ tensorProduct delta_a delta_a

    sym3_a :: Tensor Double
    sym3_a = cyclicSymmetrizeTensor 5 [0,1,2] $ tensorProduct delta_a $ tensorProduct delta_a delta_a 

    --the next functions are intended for building the equations (before stored in Main)

    --start with the first block of 16 equations

    --t1 = interArea, t2 = ivar1Tensor

    eqn1_1 :: Tensor (Double) -> Tensor (Ivar Double) ->  Tensor (Ivar Double)
    eqn1_1 intArea ivar1 = tensorContractWith_A (0,1,addIvar,zeroIvar 315) prod
            where 
                prod = tensorProductWith sMultIvar intArea ivar1 

    --intArea = interArea, ivar1 = ivar1Tensor

    eqn1_1Comps ::Tensor (Double) -> Tensor (Ivar Double) -> [[Ivar Double]]
    eqn1_1Comps intArea ivar1 = map (\x -> map tensorFlatten x) $ map (evalFullTensor [(2,0)]) t
            where
                t = evalFullTensor [(6,0),(5,0)] $ eqn1_1 intArea ivar1


    --now start extracting the next block of the pde

    --intArea = interArea, del_a = delta_a, del_A = delta_A

    eqn1_2Intertwiner :: Tensor Double -> Tensor Double -> Tensor Double -> Tensor Double
    eqn1_2Intertwiner intArea del_a del_A = tensorSub t1 t3
                where 
                    t1 =  tensorProduct intArea del_a
                    t2 = tensorTranspose 6 (0,1) $ tensorProduct del_a del_a
                    t3 = tensorProduct del_A t2

    --ivar2 = ivar2Tensor              

    eqn1_2 :: Tensor Double -> Tensor Double -> Tensor Double -> Tensor (Ivar Double) -> Tensor (Ivar Double)
    eqn1_2 intArea del_a del_A ivar2 = tensorContractWith [(0,1,addIvar,zeroIvar 315)] [] [(1,2,addIvar,zeroIvar 315)] t
                where 
                    t = tensorProductWith sMultIvar (eqn1_2Intertwiner intArea del_a del_A) ivar2 

    eqn1_2Comps :: Tensor Double -> Tensor Double -> Tensor Double -> Tensor (Ivar Double) -> [[Ivar Double]]
    eqn1_2Comps intArea del_a del_A ivar2 = map (\x -> map tensorFlatten x) $ map (evalFullTensor [(6,0),(2,0)]) t
                where
                    t = evalFullTensor [(6,0),(5,0)] $ eqn1_2 intArea del_a del_A ivar2  

    --now the third block of the first equation

    --del_I = delta_I, del_A = delta_A, intMetric = interMetric, intArea = interArea

    eqn1_3Intertwiner :: Tensor Double -> Tensor Double -> Tensor Double -> Tensor Double -> Tensor Double
    eqn1_3Intertwiner del_I del_A intMetric intArea = tensorAdd t1 t2
                where 
                    t1 = tensorProduct intArea del_I
                    t2 = tensorProduct intMetric del_A 

    --ivar3 = ivar3Tensor              

    eqn1_3 :: Tensor Double -> Tensor Double -> Tensor Double -> Tensor Double -> Tensor (Ivar Double) -> Tensor (Ivar Double)
    eqn1_3 del_I del_A intMetric intArea ivar3 = tensorContractWith [(0,1,addIvar,zeroIvar 315)] [(0,1,addIvar,zeroIvar 315)] [] t
                where 
                    t = tensorProductWith sMultIvar (eqn1_3Intertwiner del_I del_A intMetric intArea) ivar3 

    eqn1_3Comps :: Tensor Double -> Tensor Double -> Tensor Double -> Tensor Double -> Tensor (Ivar Double) -> [[Ivar Double]]
    eqn1_3Comps del_I del_A intMetric intArea ivar3 = map (\x -> map tensorFlatten x) $ map (evalFullTensor [(4,0),(2,0)]) t
                where
                    t = evalFullTensor [(6,0),(5,0)] $ eqn1_3 del_I del_A intMetric intArea ivar3

    --now the non derivative part of the equations

    --del_a = delta_a

    eqn1_4CompsNonIvar :: Tensor Double -> [Double]
    eqn1_4CompsNonIvar del_a  = map tensorFlatten $ evalFullTensor [(6,0),(5,0)] del_a

    eqn1_4Comps :: Tensor Double -> [[Ivar Double]]
    eqn1_4Comps del_a = map (\x -> [sMultIvar x l]) $ eqn1_4CompsNonIvar del_a
                     where l = mkIvar 1 (replicate 315 0) 315

    --now start with the second block of equations

    --the first 21 entries are zero

    eqn2_1Comps :: [[Ivar Double]]
    eqn2_1Comps = replicate 40 $ replicate 21 (zeroIvar 315)

    --intArea = interArea, sym2 = sym2_a, ivar1 = ivar1Tensor

    eqn2_2 :: Tensor Double -> Tensor Double -> Tensor (Ivar Double) -> Tensor (Ivar Double)
    eqn2_2 intArea sym2 ivar1 = tensorContractWith_A (0,1,addIvar,zeroIvar 315) t3
            where 
                t1 = tensorProduct intArea sym2
                t2 = tensorContract_a (0,2) t1
                t3 = tensorProductWith sMultIvar t2 ivar1 

    --now we need to take care of the symmetries when we evaluate the tensor

    eqn2_2Comps :: Tensor Double -> Tensor Double -> Tensor (Ivar Double) -> [[Ivar Double]]
    eqn2_2Comps intArea sym2 ivar1 = map (\x -> map tensorFlatten x) $ map (evalFullTensor [(6,0),(2,0)]) t2
            where
              t1 = map (\x -> evalTensor x tens) $ symEvalList2 (5,0,1)
              tens = eqn2_2 intArea sym2 ivar1
              t2 = concat $ map (evalFullTensor [(6,0)]) t1 

    --now the next block

    --intArea = interArea, int_J = inter_J, sym2 = sym2_a, del_A = delta_A, del_a = delta_a

    eqn2_3Inter :: Tensor Double -> Tensor Double -> Tensor Double -> Tensor Double -> Tensor Double -> Tensor Double
    eqn2_3Inter intArea int_J sym2 del_A del_a = tensorSub t4 r2  
         where 
              t1 = tensorProduct int_J intArea 
              t2 = tensorProduct t1 sym2
              t3 = tensorContract [] [] [(1,1),(2,2)] t2
              t4 = tensorSMult 2 t3
              r1 = tensorProduct int_J del_A
              r2 = tensorProduct del_a r1

    --ivar2 = ivar2Tensor            

    eqn2_3 :: Tensor Double -> Tensor Double -> Tensor Double -> Tensor Double -> Tensor Double -> Tensor (Ivar Double) -> Tensor (Ivar Double)
    eqn2_3 intArea int_J sym2 del_A del_a ivar2 = tensorContractWith [(0,1,addIvar, zeroIvar 315)] [] [(0,0,addIvar,zeroIvar 315)] t
         where 
                t = tensorProductWith sMultIvar (eqn2_3Inter intArea int_J sym2 del_A del_a) ivar2Tensor

    eqn2_3Comps :: Tensor Double -> Tensor Double -> Tensor Double -> Tensor Double -> Tensor Double -> Tensor (Ivar Double) -> [[Ivar Double]]
    eqn2_3Comps intArea int_J sym2 del_A del_a ivar2 = map (\x -> map tensorFlatten x) $ map (evalFullTensor [(4,0),(2,0)]) t2
          where
               t1 = map (\x -> evalTensor x tens) $ symEvalList2 (5,0,1)
               tens = eqn2_3 intArea int_J sym2 del_A del_a ivar2
               t2 = concat $ map (evalFullTensor [(6,0)]) t1 

    --again the last equation is zero

    eqn2_4Comps :: [[Ivar Double]]
    eqn2_4Comps = replicate 40 $ replicate 1 $ zeroIvar 315


    --now the last block

    eqn3_1Comps :: [[Ivar Double]]
    eqn3_1Comps = replicate 80 $ replicate 21 $ zeroIvar 315

    eqn3_2Comps :: [[Ivar Double]]
    eqn3_2Comps = replicate 80 $ replicate 84 $ zeroIvar 315

    --intArea = interArea, int_J = inter_J, sym3 = sym3_a 

    eqn3_3Inter :: Tensor Double -> Tensor Double -> Tensor Double -> Tensor Double 
    eqn3_3Inter intArea int_J sym3 = tensorContract [] [] [(0,1),(1,2),(2,3)] t2
          where 
            t1 = tensorProduct int_J intArea
            t2 = tensorProduct t1 sym3

    --ivar1 = ivar1Tensor

    eqn3_3 :: Tensor Double -> Tensor Double -> Tensor Double -> Tensor (Ivar Double) -> Tensor (Ivar Double)
    eqn3_3 intArea int_J sym3 ivar1 = tensorContractWith_A (0,1,addIvar,zeroIvar 315) t
         where 
            t = tensorProductWith sMultIvar (eqn3_3Inter intArea int_J sym3) ivar1

    eqn3_3Comps :: Tensor Double -> Tensor Double -> Tensor Double -> Tensor (Ivar Double) -> [[Ivar Double]]
    eqn3_3Comps intArea int_J sym3 ivar1 = map (\x -> map tensorFlatten x) $ map (evalFullTensor [(4,0),(2,0)]) t2
         where 
            t1 = map (\x -> evalTensor x tens) $ symEvalList3 (5,0,1,2)
            tens = eqn3_3 intArea int_J sym3 ivar1
            t2 = concat $ map (evalFullTensor [(6,0)]) t1 

    eqn3_4Comps :: [[Ivar Double]]
    eqn3_4Comps = replicate 80 $ replicate 1 $ zeroIvar 315

   