
import Data.List
import System.IO
import Index
import Tensor
import BasicTensors


test = getValue interArea (indexList [20] [6] [] [] [3] [3])

testAll = map tensorFlatten ( evalFullTensor [(1,0), (2,0), (5,0), (6,0)] interArea)

test2 = elem 3 testAll

main = do
    putStrLn $ show test2





