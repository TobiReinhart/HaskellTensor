
import Data.List
import System.IO
import qualified Data.Map.Strict as Map
import Data.Maybe
import Index
import Tensor
import BasicTensors
import Pde
import Ivar


main = do
    let delta_aMap = mkTensorMap delta_a
    let delta_IMap = mkTensorMap delta_I
    let delta_AMap = mkTensorMap delta_A

    let inter_IMap = mkTensorMap inter_I
    let inter_JMap = mkTensorMap inter_J
    let interMetricMap = mkTensorMap interMetric
    let interAreaMap = mkTensorMap interArea

    let sym2_aMap = mkTensorMap sym2_a
    let sym3_aMap = mkTensorMap sym3_a

    let ivar1TensorMap = mkTensorMap ivar1Tensor
    let ivar2TensorMap = mkTensorMap ivar2Tensor
    let ivar3TensorMap = mkTensorMap ivar3Tensor

    --now define the corresponding Tensors (where function evaluation is archieved by reading out from the maps)

    let delta_aTens = Tensor (0,0,0,0,1,1) (\x -> fromJust $ Map.lookup x delta_aMap)
    let delta_ITens = Tensor (0,0,1,1,0,0) (\x -> fromJust $ Map.lookup x delta_IMap)
    let delta_ATens = Tensor (1,1,0,0,0,0) (\x -> fromJust $ Map.lookup x delta_AMap)

    let inter_ITens = Tensor (0,0,1,0,0,2) (\x -> fromJust $ Map.lookup x inter_IMap)
    let inter_JTens = Tensor (0,0,0,1,2,0) (\x -> fromJust $ Map.lookup x inter_JMap)
    let interMetricTens = Tensor (0,0,1,1,1,1) (\x -> fromJust $ Map.lookup x interMetricMap)
    let interAreaTens = Tensor (1,1,0,0,1,1) (\x -> fromJust $ Map.lookup x interAreaMap)

    let sym2_aTens = Tensor (0,0,0,0,2,2) (\x -> fromJust $ Map.lookup x sym2_aMap)
    let sym3_aTens = Tensor (0,0,0,0,3,3) (\x -> fromJust $ Map.lookup x sym3_aMap)

    let ivar1TensorTens = Tensor (0,1,0,0,0,0) (\x -> fromJust $ Map.lookup x ivar1TensorMap)
    let ivar2TensorTens = Tensor (0,1,0,0,0,1) (\x -> fromJust $ Map.lookup x ivar2TensorMap)
    let ivar3TensorTens = Tensor (0,1,0,1,0,0) (\x -> fromJust $ Map.lookup x ivar3TensorMap)

    --now we have all tensors

    let test = eqn1_3Comps delta_ITens delta_ATens interMetricTens interAreaTens ivar3TensorTens

    writeFile "DiffeoEqnsHaskell1.txt" $ show test 









