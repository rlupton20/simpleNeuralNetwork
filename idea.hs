
-- A layer is a collection of neurons all with the same number of inputs
-- We use this number as a descriptor for the layer
type Layer = Int

type Neuron = (Float, [Float])

type Network = [Float] -> [Float]

buildNeur :: [Float] -> Neuron
buildNeur [] = (0, [])
buildNeur (x:xs) = (x,xs)

buildLayer :: Layer -> [Float] -> Network
buildLayer n pars = \ins -> map (fire ins) lyr
                            where lyr = assemble n pars
                                  assemble 0 _ = []
                                  assemble _ [] = []
                                  assemble n xs = buildNeur (take (n+1) xs) : assemble n (drop (n+1) xs)

fire :: [Float] -> Neuron -> Float
fire inputs (bias, weights) = 1 / (1 + exp (-dot-bias))
	where dot = sum $ zipWith (*) inputs weights
