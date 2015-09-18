data Neuron = Neuron Float [Float] deriving (Eq, Show)

fire :: [Float] -> Neuron -> Float
fire inputs (Neuron bias weights) = 1 / (1 + exp (-dot - bias))
	where dot = sum $ zipWith (*) inputs weights
