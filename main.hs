import Data.Array

data Neuron = Neuron Float [Float] deriving (Eq, Show, Read)

type Layer = [Neuron]

startNode :: Int -> Neuron
startNode n = Neuron 0 (replicate n 0)

fire :: [Float] -> Neuron -> Float
fire inputs (Neuron bias weights) = 1 / (1 + exp (-dot - bias))
	where dot = sum $ zipWith (*) inputs weights

nextLayer :: Layer -> Int -> Layer
nextLayer nes n = replicate n (startNode m)
                  where m = length nes
