
-- A layer has a length and an arity
type Layer = (Int, Int)

type Neuron = (Float, [Float])

type Network = [Float] -> [Float]

buildNeur :: [Float] -> Neuron
buildNeur [] = (0, [])
buildNeur (x:xs) = (x,xs)

buildLayer :: Layer -> [Float] -> Network
buildLayer (len, ar) pars = \ins -> map (fire ins) lyr
                            where lyr = assemble (len, ar) pars
                                  assemble (_, 0) _ = []
                                  assemble (0, ar) _ = []
                                  assemble (len, ar) xs = buildNeur (take (ar+1) xs) : assemble (len-1, ar) (drop (ar+1) xs)

fire :: [Float] -> Neuron -> Float
fire inputs (bias, weights) = 1 / (1 + exp (-dot-bias))
	where dot = sum $ zipWith (*) inputs weights
