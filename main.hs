import Data.Array

type Neuron = (Float, [Float])
type Layer = [Neuron]

type Network = [Layer]

startNode :: Int -> Neuron
startNode n = (0, (replicate n 0))

fire :: [Float] -> Neuron -> Float
fire inputs (bias, weights) = 1 / (1 + exp (-dot-bias))
	where dot = sum $ zipWith (*) inputs weights

nextLayer :: Layer -> Int -> Layer
nextLayer nes n = replicate n (startNode m)
                  where m = length nes

step :: [Float] -> Layer -> [Float]
step ins layer = map (fire ins) $ layer

addLayer :: Layer -> Network -> Network
addLayer l n = l:n

runNetwork :: Network -> [Float] -> [Float]
runNetwork net ins = foldr (flip step) ins net
