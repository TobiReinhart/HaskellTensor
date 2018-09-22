
module Tensor (
Rank, Tensor(..), 
checkIndex, getRank, getTensorFunction, getValue, getRangeList, getIndexRange, mkTensorMap,
tensorSMult, tensorAdd, tensorSub, tensorTranspose, tensorBlockTranspose, symmetrizeTensor,aSymmetrizeTensor,
blockSymmetrizeTensor, ordSubLists2, cyclicSymmetrizeTensor, symTensor, rankPlus, rankMinus, safeSplitAt,
splitInds, tensorProduct, tensorProductWith, tensorContract_A, tensorContract_I, tensorContract_a, tensorContract,
tensorContractWith_a, tensorContractWith_I, tensorContractWith_A, tensorContractWith,
rankReduce, evalTensor1, evalTensor, rankLength, tensorFlatten, evalFullTensor1, evalFullTensor,
evalRangeTensor1, evalRangeTensor
    ) where 
    
    
    import Data.List
    import qualified Data.Map.Strict as Map
    import Data.Tuple
    import Index
    import System.IO
    
    --the first step is to define the rank in teh form (U_A,L_A,U_I,L_I,U_a,L_a)
    
    type Rank = (Int,Int,Int,Int,Int,Int)
    
    --check if a given index fits the valence of a tensor (this function checks if the given Index is valid for the given tensor)
    
    --should be built in the constructor of tensor

    --this is slow !!!!!!!!!!!!!!!
    
    checkIndex :: Index -> Rank -> Bool
    checkIndex (i1,i2,i3,i4,i5,i6) (r1,r2,r3,r4,r5,r6) 
            | length i1 == r1 && length i2 == r2 && length i3 == r3 && length i4 == r4 && length i5 == r5 && length i6 == r6 = True
            | otherwise = False
            
    --now define the tensor datatype
    
    data Tensor a = Tensor Rank (Index -> a)

    --this allows us to apply a function to each value stored in a Tensor

    instance Functor Tensor where
        fmap f (Tensor rank g) = Tensor rank (f.g)
    
    getRank :: (Tensor a) -> Rank
    getRank (Tensor rank f) = rank
    
    getTensorFunction :: (Tensor a) -> (Index -> a)
    getTensorFunction (Tensor rank f) = f
    
    --everytime we read out values we check if the index fits the rank (this function is for full evaluation)

    --values should always be read out using getValue !!! (if it is not too slow)

    --it is slow !!!!!!!!!!!!!
    
    getValue :: (Tensor a) -> Index -> a
    getValue (Tensor rank f) ind 
            | checkIndex ind rank = f ind
            | otherwise = error "Tensor is evaluated at wrong index"
    
    getRangeList :: Int -> Int -> [[Int]]
    getRangeList i r
            | r <= 1 = error "wrong index range"
            | i < 0 = error "negative number of indices"
            | i == 0 = [[]]
            | i == 1 = [[a]| a <- [0..r-1]]
            | otherwise = [ a++b | a <- [[a]| a <- [0..r-1]], b <- getRangeList (i-1) r]  
    
    
    getIndexRange :: Rank -> [Index]
    getIndexRange (a,b,c,d,e,f) = map (\(r,s,t,u,v,w) -> indexList r s t u v w ) list
            where 
                    list = [ (l,m,n,o,p,q) | l <- (getRangeList a 21), m <- (getRangeList b 21), n <- (getRangeList c 10),
                            o <- (getRangeList d 10), p <- (getRangeList e 4), q <- (getRangeList f 4)]
    
    mkTensorMap :: (Tensor a) -> (Map.Map Index a) 
    mkTensorMap (Tensor rank f) = Map.fromList ( zip indList valueList )
            where 
                    indList = getIndexRange rank 
                    valueList = map (getValue (Tensor rank f)) indList
    
    --now start defining teh funcitons for the tensor algebra
    
    tensorSMult :: (Num a) => a -> Tensor a -> Tensor a
    tensorSMult i (Tensor rank f) = Tensor rank g
                    where g = ((*) i).(getValue (Tensor rank f))
    
    tensorAdd :: (Num a) => Tensor a -> Tensor a -> Tensor a
    tensorAdd (Tensor rank1 f) (Tensor rank2 g) 
                    | rank1 == rank2 = Tensor rank1 h
                    | otherwise = error "cannot add tensors of different rank"
                    where    h = \x -> ((getValue (Tensor rank2 g)) x) + ((getValue (Tensor rank1  f)) x)
                    
    tensorSub :: (Num a) => Tensor a -> Tensor a -> Tensor a
    tensorSub (Tensor rank1 f) (Tensor rank2 g) 
                    | rank1 == rank2 = Tensor rank1 h
                    | otherwise = error "cannot add tensors of different rank"
                    where    h = \x -> ((getValue (Tensor rank2  g))  x) - ((getValue (Tensor rank1  f))  x) 
    
                    
    --next we define a function that transposes a tensor, i.e. swoops its indices

    --does (Int,Int) start at 0 or 1?????!!!

    --probably 0
    
    tensorTranspose :: Int -> (Int,Int) -> Tensor a -> Tensor a
    tensorTranspose i j (Tensor rank f) = (Tensor rank g)
            where g = ((getValue (Tensor rank  f)) ).(interchangeInds i j)  
    
    tensorBlockTranspose :: Int -> ([Int],[Int]) -> Tensor a -> Tensor a
    tensorBlockTranspose i j (Tensor rank f) = (Tensor rank g)
            where g = (getValue (Tensor rank f)).(interchangeBlockInds i j)    
            
    --we need functions for symmetrizing tensors

    symmetrizeTensor :: (Fractional a) => Int -> (Int,Int) -> Tensor a -> Tensor a
    symmetrizeTensor n k t = tensorSMult (1/2) (tensorAdd t (tensorTranspose n k t))

    aSymmetrizeTensor :: (Fractional a) => Int -> (Int,Int) -> Tensor a -> Tensor a
    aSymmetrizeTensor n k t = tensorSMult (1/2) (tensorSub t (tensorTranspose n k t))

    blockSymmetrizeTensor :: (Fractional a) => Int -> ([Int],[Int]) -> Tensor a -> Tensor a
    blockSymmetrizeTensor n k t = tensorSMult (1/2) (tensorAdd t (tensorBlockTranspose n k t))

    --now cyclic symmetrization

    ordSubLists2 :: [a] -> [(a,a)]
    ordSubLists2 x 
        | lengthX < 2 = []
        | lengthX == 2 = [(x !! 0, x !! 1)]
        | otherwise = zip (repeat (head x)) (tailX) ++ ordSubLists2 (tailX)
          where lengthX = length x
                tailX = tail x

    cyclicSymmetrizeTensor :: (Fractional a) => Int -> [Int] -> Tensor a -> Tensor a
    cyclicSymmetrizeTensor  n k t = foldr  (symmetrizeTensor n) t list2
                where list2 = ordSubLists2 k 

   --combine the symmetrizer functions in the form syms, asyms ,blocksyms, cyclcicsyms

    symTensor :: (Fractional a) => Int -> [(Int,Int)] -> [(Int,Int)] -> [([Int],[Int])] -> [[Int]] -> Tensor a -> Tensor a
    symTensor n syms asyms bsyms csyms t = (tensorCSym.tensorBSym.tensorASym.tensorSym) t 
                where 
                        tensorSym = \x1 -> foldr (symmetrizeTensor n) x1 syms
                        tensorASym = \x2 -> foldr (aSymmetrizeTensor n) x2 asyms
                        tensorBSym = \x3 -> foldr (blockSymmetrizeTensor n) x3 bsyms
                        tensorCSym = \x4 -> foldr (cyclicSymmetrizeTensor n) x4 csyms


    --the next step is writing a tensor product function

    rankPlus :: Rank -> Rank -> Rank
    rankPlus (a,b,c,d,e,f) (g,h,i,j,k,l) = (a+g,b+h,c+i,d+j,e+k,f+l)

    rankMinus :: Rank -> Rank -> Rank 
    rankMinus (a,b,c,d,e,f) (g,h,i,j,k,l) = (a-g,b-h,c-i,d-j,e-k,f-l)

    --there is the problem !!!

    --determining the length makes it probably slow

    --but it makes it safer

    safeSplitAt :: (Eq a) =>  Int -> [a] -> ([a],[a])
    safeSplitAt i l 
                | i > length l = error "not enough elements in list"
                | otherwise = splitAt i l

    --there is a problem!!!           

    splitInds :: Rank -> Index -> (Index,Index)
    splitInds (r1,r2,r3,r4,r5,r6) (i1,i2,i3,i4,i5,i6) = 
        ((fst split1, fst split2, fst split3, fst split4, fst split5, fst split6),
         (snd split1, snd split2, snd split3, snd split4, snd split5, snd split6))
                where 
                    split1 = safeSplitAt r1 i1
                    split2 = safeSplitAt r2 i2
                    split3 = safeSplitAt r3 i3
                    split4 = safeSplitAt r4 i4
                    split5 = safeSplitAt r5 i5
                    split6 = safeSplitAt r6 i6

    --there is a problem with lists starting at zero !!!!!!

    tensorProduct :: (Num a) => Tensor a -> Tensor a -> Tensor a
    tensorProduct (Tensor rank1 f) (Tensor rank2 g) = Tensor (rankPlus rank1 rank2) h
                 where 
                    h = \x -> (getValue (Tensor rank1  f)  (fst (split x))) * (getValue (Tensor rank2  g)  (snd (split x)))
                    split = splitInds rank1

    --TensorProdukt with combiner function, for tensors with different typed entries
                    
    tensorProductWith ::   (a -> b -> c ) -> Tensor a -> Tensor b -> Tensor c
    tensorProductWith combine (Tensor rank1 f) (Tensor rank2 g) = Tensor (rankPlus rank1 rank2) h
                 where 
                    h = \x -> combine (getValue (Tensor rank1  f)  (fst (split x)))  (getValue (Tensor rank2  g)  (snd (split x)))
                    split = splitInds rank1

    --and the contraction of indices

    tensorContract_A :: (Num a) => (Int,Int) -> Tensor a -> Tensor a 
    tensorContract_A k (Tensor rank f) = Tensor newrank g
                        where 
                                newrank = rankMinus rank (1,1,0,0,0,0)
                                g = \x -> foldl (+) 0 (map (getValue (Tensor rank  f))  (contractionIndex_A k x))


    tensorContract_I :: (Num a) => (Int,Int) -> Tensor a -> Tensor a 
    tensorContract_I k (Tensor rank f) = Tensor newrank g
                        where 
                                newrank = rankMinus rank (0,0,1,1,0,0)
                                g = \x -> foldl (+) 0 (map (getValue (Tensor rank  f))  (contractionIndex_I k x))

    tensorContract_a :: (Num a) => (Int,Int) -> Tensor a -> Tensor a 
    tensorContract_a k (Tensor rank f) = Tensor newrank g
                        where 
                                newrank = rankMinus rank (0,0,0,0,1,1)
                                g = \x -> foldl (+) 0 (map (getValue (Tensor rank  f))  (contractionIndex_a k x))

    --best to enter the indices thta are contracted beginning with the last index   

    --carefull as there was a problem (lists [(Int,Int)] are evaluated from right to left) !!

    tensorContract :: (Num a) => [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)] -> Tensor a -> Tensor a
    tensorContract inds_A inds_I inds_a t = (c_A.c_I.c_a) t
                                where
                                        c_A = \x1 -> foldr tensorContract_A x1 inds_A 
                                        c_I = \x2 -> foldr tensorContract_I x2 inds_I
                                        c_a = \x3 -> foldr tensorContract_a x3 inds_a




    --in order to deal with the Ivar entries in the Eqns we need to be able to Contract Tensor with elements that are no instance of Num
    --i.e. where the + function is constructed by hand


    --position specified in (Int,Int) starts at 0 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    --same in evalTensor !!!!

    tensorContractWith_A :: (Int,Int,a -> a -> a,a) -> Tensor a -> Tensor a 
    tensorContractWith_A (i,j,plus,zero) (Tensor rank f) = Tensor newrank g
                        where 
                                newrank = rankMinus rank (1,1,0,0,0,0)
                                g = \x -> foldl plus zero (map (getValue (Tensor rank  f))  (contractionIndex_A (i,j) x))

    tensorContractWith_I ::  (Int,Int,a -> a -> a, a) -> Tensor a -> Tensor a 
    tensorContractWith_I (i,j,plus,zero) (Tensor rank f) = Tensor newrank g
                        where 
                                newrank = rankMinus rank (0,0,1,1,0,0)
                                g = \x -> foldl plus zero (map (getValue (Tensor rank  f))  (contractionIndex_I (i,j) x))

    tensorContractWith_a ::  (Int,Int,a -> a -> a, a) -> Tensor a -> Tensor a 
    tensorContractWith_a (i,j,plus,zero) (Tensor rank f) = Tensor newrank g
                        where 
                                newrank = rankMinus rank (0,0,0,0,1,1)
                                g = \x -> foldl plus zero (map (getValue (Tensor rank  f))  (contractionIndex_a (i,j) x))


    tensorContractWith ::  [(Int,Int,a -> a -> a, a)] -> [(Int,Int,a -> a -> a, a)] -> [(Int,Int,a -> a -> a, a)] -> Tensor a -> Tensor a
    tensorContractWith inds_A inds_I inds_a t = (c_A.c_I.c_a) t
                                where
                                        c_A = \x1 -> foldr tensorContractWith_A x1 inds_A 
                                        c_I = \x2 -> foldr tensorContractWith_I x2 inds_I
                                        c_a = \x3 -> foldr tensorContractWith_a x3 inds_a


    --furthermore we need a function that allows us to insert specific values for some of the indices of a given tensor
    --and as result get a tensor of reduced rank

    --the 3 Ints specify the index slot the position and the value that is inerted

    rankReduce :: Int -> Rank -> Rank
    rankReduce i (a,b,c,d,e,f) 
        | i <= 0 || i> 6 = error "index not in rank range"
        | i == 1 = (a-1,b,c,d,e,f)
        | i == 2 = (a,b-1,c,d,e,f)
        | i == 3 = (a,b,c-1,d,e,f)
        | i == 4 = (a,b,c,d-1,e,f)
        | i == 5 = (a,b,c,d,e-1,f)
        | i == 6 = (a,b,c,d,e,f-1)

    --evals Tensor at one specific index

    evalTensor1 :: (Int,Int,Int) -> Tensor a -> Tensor a
    evalTensor1 (i,j,k) (Tensor rank f) = Tensor newrank g
        where 
                newrank = rankReduce i rank
                g = \x -> (getValue (Tensor rank  f))  (insertIndex i j k x)
   
    --evals Tensor at multiple indicesspecified by the Int triples succesively (obviously only works for multiple indeices in the list)

    --again as with contractions list are folded from right to left 

    evalTensor :: [(Int,Int,Int)] -> Tensor a -> Tensor a
    evalTensor k t = foldr evalTensor1 t k 

    --we also need a fucntion to evaluat tensors with symmetries

    --this can be done with evalTensor with the appropriate lists

    --the next function is for evaluating a tensor in the full range of the indices that correspond to a given list of slots

    --careful as evalTensor removes indices of the tensor and thereby possibly changes the position of indices (aboce the same)

    evalFullTensor1 :: (Int,Int) -> Tensor a -> [Tensor a]
    evalFullTensor1 (i,j) t 
                | i == 1 || i == 2 = map (\x -> evalTensor1 x t) (zipWith g (replicate 21 (i,j)) [0..20])
                | i == 3 || i == 4 = map (\x -> evalTensor1 x t) (zipWith g (replicate 10 (i,j)) [0..9])
                | i == 5 || i == 6 = map (\x -> evalTensor1 x t) (zipWith g (replicate 4 (i,j)) [0..3])
                | otherwise = error "wrong index slot to insert value"
                 where g (a,b) n = (a,b,n) 

    evalFullTensor ::  [(Int,Int)] -> Tensor a -> [Tensor a]
    evalFullTensor k t = foldr g [t] k
                where g = \x y -> concat (map (evalFullTensor1 x) y )

    --the last eval function evals the tensor only in a restricted range of some of its indices (i.e when symmetries are present)

    evalRangeTensor1 :: (Int,Int,[Int]) -> Tensor a -> [Tensor a]
    evalRangeTensor1 (m,n,j) t = map (\x -> evalTensor1 x t) (zipWith g (replicate (length j) (m,n)) j)
                where g (a,b) n = (a,b,n)

    evalRangeTensor :: [(Int,Int,[Int])] -> Tensor a -> [Tensor a]
    evalRangeTensor k t = foldr g [t] k
                where g = \x y -> concat (map (evalRangeTensor1 x) y )

    rankLength :: Rank -> Int
    rankLength (a,b,c,d,e,f) = a + b + c + d + e + f

    tensorFlatten :: Tensor a -> a
    tensorFlatten (Tensor rank f) 
                | rankLength rank /= 0 = error "can only flatten tensors of rank 1"
                | otherwise = (getValue (Tensor rank  f))  ([],[],[],[],[],[])



