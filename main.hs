-- A layer has a length and an arity
type Layer = (Int, Int)

type Neuron = (Float, [Float])

-- This should develop as more structures become possible
-- For the moment though a network is just a list of layers
-- all outputs from a previous layer are fed into each neuron
-- in the next.
type NetworkType = [Layer]

type UnrolledNetwork = (NetworkType, [Float])

type Network = [Float] -> [Float]

buildNeur :: [Float] -> Neuron
buildNeur [] = (0, [])
buildNeur (x:xs) = (x,xs)

-- Note that we can now build a network by composing layers
buildLayer :: Layer -> [Float] -> Network
buildLayer (len, ar) pars = \ins -> map (fire ins) lyr
                            where lyr = assemble (len, ar) pars
                                  assemble (_, 0) _ = []
                                  assemble (0, ar) _ = []
                                  assemble (len, ar) xs = buildNeur (take (ar+1) xs) : assemble (len-1, ar) (drop (ar+1) xs)

fire :: [Float] -> Neuron -> Float
fire inputs (bias, weights) = 1 / (1 + exp (-dot-bias))
	where dot = sum $ zipWith (*) inputs weights

buildNetwork :: UnrolledNetwork -> Network
buildNetwork ([], _)  = id
buildNetwork (_, []) = id
buildNetwork ((l:ls), pars) = rest . layer
  where layer = buildLayer l $ take numpars pars
        rest = buildNetwork (ls, drop numpars pars)
        numpars = len * (ar + 1)
        (len, ar) = l

newNetwork :: NetworkType -> UnrolledNetwork
newNetwork ls = (ls, zeros)
                where zeros = replicate numzeros 0
                      numzeros = foldr (\l acc -> acc + numpars l) 0 ls
                      numpars (len, ar) = len * (ar + 1)

adjust :: UnrolledNetwork -> [Float] -> UnrolledNetwork
adjust (l, pars) fs = (l, zipWith (+) pars (fs ++ repeat 0))

simpleNetwork :: [Int] -> UnrolledNetwork
simpleNetwork ls@(ins:_) = newNetwork $ simpleType ins ls
  where simpleType _ [] = [(0,0)]
        simpleType i [n] = [(n, i)]
        simpleType i (n:nss) = (n,i) : simpleType n nss
