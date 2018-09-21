
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

test = getValue ivar1Tensor (indexList [] [0] [] [] [] [] )

main = do
    putStrLn $ show (test)



