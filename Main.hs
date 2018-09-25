
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

    --construct the equations

    let e1_1 = eqn1_1Comps interAreaTens ivar1TensorTens
    let e1_2 = eqn1_2Comps interAreaTens delta_aTens delta_ATens ivar2TensorTens
    let e1_3 = eqn1_3Comps delta_ITens delta_ATens interMetricTens interAreaTens ivar3TensorTens
    let e1_4 = eqn1_4Comps delta_aTens

    let e2_1 = eqn2_1Comps
    let e2_2 = eqn2_2Comps interAreaTens sym2_aTens ivar1TensorTens
    let e2_3 = eqn2_3Comps interAreaTens inter_JTens sym2_aTens delta_ATens delta_aTens ivar2TensorTens
    let e2_4 = eqn2_4Comps

    let e3_1 = eqn3_1Comps
    let e3_2 = eqn3_2Comps
    let e3_3 = eqn3_3Comps interAreaTens inter_JTens sym3_aTens ivar1Tensor
    let e3_4 = eqn3_4Comps

    --combine the equaitons (for the moment as lists)

    let e1 = zipWith4 (\a b c d -> a++b++c++d) e1_1  e1_2  e1_3  e1_4
    let e2 = zipWith4 (\a b c d -> a++b++c++d) e2_1  e2_2  e2_3  e2_4
    let e3 = zipWith4 (\a b c d -> a++b++c++d) e3_1  e3_2  e3_3  e3_4

    let equation = e1++e2++e3

    --now start constructing the pde

    let multIndList = mkAllMultiIndsUptoRev 315 1
    let lDiffList = zip multIndList $ repeat 1

    --trasform the pde list into the correct form

    --the biggest problem is to check if we evaluated ivars tensor inds and multinds the same way

    let pdeList = map (\x -> zip lDiffList x) equation

    --now construct the pde

    let pdeSys = map (mkPde (zeroIvar 315) 1 315 1) pdeList 

    --now prolong the system

    let eqnDerProd = [ (a,b) | a <- (mkAllMultiInds 315 1), b <- pdeSys ]

    --this is the full prolongation

    let prolongSys = (map (\x -> (prolongPdeIvar (fst x) (snd x))) eqnDerProd) ++ pdeSys

    --in the symbol only the highest derivatives are stored

    let prolongSymbol = (map (\x -> (prolongPdeConstCoeff (fst x) (snd x))) eqnDerProd)


    --the only thing missing right now is removing duplicated computations (e.g. multind1tonumber via maps !!!)

    sysread <- readFile "HaskellPdeSys.txt"

    let pdeSysSaved = readPdeSys sysread

    let pdeSysList = map (\x -> zip lDiffList x) pdeSysSaved

    let pdeSys2 = map (mkPde (zeroIvar 315) 1 315 1) pdeSysList
  
    let test = prolongSystem (take 10 $ mkAllMultiInds 315 1) pdeSys2
    
    putStrLn $ show $ printSystoMaple test

    --it seems to work

    --too slow 









