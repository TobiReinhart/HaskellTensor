
import Data.List
import System.IO
import Index
import Tensor
import Involutive

deltaF_a :: (Num a) => Index -> a
deltaF_a ([],[],[],[],a,b) 
    | length a == 1 && length b == 1 && toListfromU_a a == toListfromL_a b = 1
    |  length a == 1 && length b == 1 = 0
    | otherwise = error "wrong number of indices"
deltaF_a ind = error "delta has only 2 indices"

delta_a :: (Num a) => Tensor a
delta_a = Tensor (0,0,0,0,1,1) deltaF_a

delta_aComps :: (Num a) => [Tensor a]
delta_aComps = evalFullTensor [(5,0),(6,0)] delta_a

delta_aCompList :: (Num a) => [a]
delta_aCompList = map tensorFlatten delta_aComps

deltaProd :: (Num a) => Tensor a
deltaProd = tensorProduct delta_a delta_a

val :: (Num a) => a
val = getValue deltaProd ([],[],[],[],fromList2U_a [1,0], fromList2L_a [1,0])

val2 :: (Num a) => a
val2 = getValue deltaContraction ([],[],[],[],fromList2U_a [1], fromList2L_a [0])


deltaContraction :: (Num a) => Tensor a
deltaContraction = tensorContract_a (1,0) deltaProd

deltaComps2 :: (Num a) => [Tensor a]
deltaComps2 = evalFullTensor [(5,0),(6,0)] deltaContraction

deltaCompList2 :: (Num a) => [a]
deltaCompList2 = map tensorFlatten deltaComps2
 


 



main =  do
    putStrLn $ show val2
