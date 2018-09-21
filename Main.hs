
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




main = do
    putStrLn $ show $ eqn1_3Comps !! 0



--it works but is super slow !!!