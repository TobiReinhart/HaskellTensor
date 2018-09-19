
import Data.List
import System.IO
import Index
import Tensor
import Involutive

test = tensorContract_a (0,0) delta_a

test2 = tensorFlatten test

test3 = map tensorFlatten (evalFullTensor [(5,0),(6,0)] delta_a)

test4 = map tensorFlatten $  evalFullTensor [(5,0),(6,0)] $tensorContract [] [] [] delta_a 

test5 = interArea

main = do
    putStrLn $ show test4





