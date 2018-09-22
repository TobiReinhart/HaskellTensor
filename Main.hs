
import Data.List
import System.IO
import Index
import Tensor
import BasicTensors
import Pde
import Ivar

--the first step is extracting the equations in list form

--start with the first block of 16 equations


eqn1_1 :: (Fractional a) => Tensor (Ivar a)
eqn1_1 = tensorContractWith_A (0,1,addIvar,zeroIvar 315) prod
        where 
            t1 = interArea
            t2 = ivar1Tensor
            prod = tensorProductWith sMultIvar t1 t2 

eqn1_1Comps :: (Fractional a) => [[Ivar a]]
eqn1_1Comps = map (\x -> map tensorFlatten x) $ map (evalFullTensor [(2,0)]) t
        where
            t = evalFullTensor [(6,0),(5,0)] eqn1_1   


--now start extracting the next block of the pde

eqn1_2Intertwiner :: (Fractional a) => Tensor a
eqn1_2Intertwiner = tensorSub t1 t3
            where 
                t1 =  tensorProduct interArea delta_a
                t2 = tensorTranspose 6 (0,1) $ tensorProduct delta_a delta_a
                t3 = tensorProduct delta_A t2

eqn1_2 :: (Fractional a) => Tensor (Ivar a)
eqn1_2 = tensorContractWith [(0,1,addIvar,zeroIvar 315)] [] [(1,2,addIvar,zeroIvar 315)] t
            where 
                t = tensorProductWith sMultIvar eqn1_2Intertwiner ivar2Tensor 

eqn1_2Comps :: (Fractional a) => [[Ivar a]]
eqn1_2Comps = map (\x -> map tensorFlatten x) $ map (evalFullTensor [(2,0),(6,0)]) t
            where
                t = evalFullTensor [(6,0),(5,0)] eqn1_2   

--now the third block of the first equation

eqn1_3Intertwiner :: (Fractional a) => Tensor a
eqn1_3Intertwiner = tensorAdd t1 t2
            where 
                t1 = tensorProduct interArea delta_I
                t2 = tensorProduct interMetric delta_A 

--there is the problem                

eqn1_3 :: (Fractional a) => Tensor (Ivar a)
eqn1_3 = tensorContractWith [(0,1,addIvar,zeroIvar 315)] [(0,1,addIvar,zeroIvar 315)] [] t
            where 
                t = tensorProductWith sMultIvar eqn1_3Intertwiner ivar3Tensor 

eqn1_3Comps :: (Fractional a) => [[Ivar a]]
eqn1_3Comps = map (\x -> map tensorFlatten x) $ map (evalFullTensor [(2,0),(4,0)]) t
            where
                t = evalFullTensor [(6,0),(5,0)] eqn1_3   

--now the non derivative part of the equations

eqn1_4CompsNonIvar :: (Fractional a) => [a]
eqn1_4CompsNonIvar = map tensorFlatten $ evalFullTensor [(6,0),(5,0)] delta_a

eqn1_4Comps :: (Fractional a) => [[Ivar a]]
eqn1_4Comps = map (\x -> [sMultIvar x l]) eqn1_4CompsNonIvar
                where l = mkIvar 1 (replicate 315 0) 315

--combine the equations (for the moment as a list)

eqn1 :: (Fractional a) => [[Ivar a]]
eqn1 = zipWith4 (\a b c d -> a ++ b ++ c ++ d) eqn1_1Comps eqn1_2Comps eqn1_3Comps eqn1_4Comps
        
--it works but is super slow !!!

--now start with the second block of equations

--the first 21 entries are zero

eqn2_1Comps :: (Fractional a) => [[Ivar a]]
eqn2_1Comps = replicate 40 $ replicate 21 (zeroIvar 315)

eqn2_2 :: (Fractional a) => Tensor (Ivar a)
eqn2_2 = tensorContractWith_A (0,1,addIvar,zeroIvar 315) t3
        where 
            t1 = tensorProduct interArea sym2_a
            t2 = tensorContract_a (0,2) t1
            t3 = tensorProductWith sMultIvar t2 ivar1Tensor 

--now we need to take care of the symmetries when we evaluate the tensor

eqn2_2Comps :: (Fractional a) => [[Ivar a]]
eqn2_2Comps = map (\x -> map tensorFlatten x) $ map (evalFullTensor [(2,0),(6,0)]) t2
        where
             t1 = map (\x -> evalTensor x eqn2_2) $ symEvalList2 (5,0,1)
             t2 = concat $ map (evalFullTensor [(6,0)]) t1 

--now the next block

eqn2_3Inter :: (Fractional a) => Tensor a
eqn2_3Inter = tensorSub t4 r2  
        where 
            t1 = tensorProduct inter_J interArea 
            t2 = tensorProduct t1 sym2_a
            t3 = tensorContract [] [] [(1,1),(2,2)] t2
            t4 = tensorSMult 2 t3
            r1 = tensorProduct inter_J delta_A
            r2 = tensorProduct delta_a r1

eqn2_3 :: (Fractional a) => Tensor (Ivar a)
eqn2_3 = tensorContractWith [(0,1,addIvar, zeroIvar 315)] [] [(0,0,addIvar,zeroIvar 315)] t
        where 
            t = tensorProductWith sMultIvar eqn2_3Inter ivar2Tensor

eqn2_3Comps :: (Fractional a) => [[Ivar a]]
eqn2_3Comps = map (\x -> map tensorFlatten x) $ map (evalFullTensor [(2,0),(4,0)]) t2
        where
             t1 = map (\x -> evalTensor x eqn2_3) $ symEvalList2 (5,0,1)
             t2 = concat $ map (evalFullTensor [(6,0)]) t1 

--again the last equation is zero

eqn2_4Comps :: (Fractional a) => [[Ivar a]]
eqn2_4Comps = replicate 40 $ replicate 210 $ zeroIvar 315

--combinen the blocks to one equations

eqn2 :: (Fractional a) => [[Ivar a]]
eqn2 = zipWith4 (\a b c d -> a ++ b ++ c ++ d) eqn2_1Comps eqn2_2Comps eqn2_3Comps eqn2_4Comps




main = do
    putStrLn $ show $ length eqn2
