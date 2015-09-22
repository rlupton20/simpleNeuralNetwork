import qualified Data.Vector as V
import Data.Vector (Vector)

-- A layer has a length and an arity
type Layer = (Int, Int)
type Neuron = (Float, Vector Float)

-- This should develop as more structures become possible
-- For the moment though a network is just a list of layers
-- all outputs from a previous layer are fed into each neuron
-- in the next.
type NetworkType = [Layer]
type UnrolledNetwork = (NetworkType, Vector Float)
type Network = Vector Float -> Vector Float

buildNeur :: Vector Float -> Neuron
buildNeur pars
  | V.null pars = (0, V.empty)
  | otherwise  = (V.head pars, V.tail pars)

fire :: Vector Float -> Neuron -> Float
fire inputs (bias, weights) = 1 / (1 + exp (-dotprod-bias))
	where dotprod = V.sum $ V.zipWith (*) inputs weights

buildLayerNeurs :: Layer -> Vector Float -> Vector Neuron
buildLayerNeurs (len, arity) pars
  | len == 0 = V.empty
  | otherwise = (buildNeur $ V.take (arity + 1) pars) `V.cons` (buildLayerNeurs (len-1, arity) $ V.drop (arity + 1) pars)

layerToNetwork :: Layer -> Vector Float -> Network
layerToNetwork layer pars = \inputs -> V.map (fire inputs) (buildLayerNeurs layer pars)

-- Not sure if this is really needed, but it might be useful later
-- This function assembles each individual layer sequentially
buildLayers :: UnrolledNetwork -> [Network]
buildLayers ([], _) = []
buildLayers ((l:ls) , pars) = let (len, arity) = l; numPars = len * (arity + 1) in
  layerToNetwork l (V.take numPars pars) : buildLayers (ls, V.drop numPars pars)

buildNetwork :: UnrolledNetwork -> Network
buildNetwork = foldr (\f acc -> acc . f) id . buildLayers

newNetwork :: NetworkType -> UnrolledNetwork
newNetwork ls = (ls, zeros)
                where zeros = V.replicate numzeros 0
                      numzeros = foldr (\l acc -> acc + numpars l) 0 ls
                      numpars (len, ar) = len * (ar + 1)

simpleNetwork :: [Int] -> UnrolledNetwork
simpleNetwork ls@(ins:_) = newNetwork $ simpleType ins ls
  where simpleType _ [] = [(0,0)]
        simpleType i [n] = [(n, i)]
        simpleType i (n:nss) = (n,i) : simpleType n nss
