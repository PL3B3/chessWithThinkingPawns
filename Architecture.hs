module Architecture (
	
) where

import LinearAlgebra

type Layer = ((Acti, Flavor), [Tensor Double])

type Trans = ((Flavor, String), [String]) deriving Show

type Pointer = (Trans, Trans)

data Flow = Linear | Repeat deriving Show
data Acti = Tanh | Sigmoid | Relu deriving Show
data Flavor = In | Out | Feed | Recur | Convol | Split | Merge deriving Show


name :: Trans -> String
name trans@((f, n), data) = n

flavor :: Trans -> Flavor
flavor trans@((f, n), data) = f

filterFrom :: [Pointer] -> String -> [Pointer]
filterFrom pList name = filter (\x -> name == snd x) pList

filterToward :: [Pointer] -> [Pointer]
filterToward pList name = filter (\x -> name == fst x) pList

crossEntropy :: Double -> Double -> Double
crossEntropy activation target = negate $ (target * (log activation)) + ((1.0 - target) * (log $ 1.0 - activation)) 

sigmoid :: Double -> Double
sigmoid number = 1.0 / (1.0 + (exp number))

sigmoidD :: Double -> Double
sigmoidD number = negate $ (exp number) * ((1 + (exp number)) ^ (-2))

type Transform a = ((Tensor a -> Tensor a), (Tensor a -> Tensor a)) 

feed :: (Num a) => Tensor a -> Tensor a -> Tensor a -> Tensor a 
feed input weight bias = ta (mm input (sd weight [1,0])) bias

feed2 :: (Num a) => Tensor a -> Tensor a -> (a -> a) -> (Tensor a -> Tensor a)
feed2 w b a = (\i -> fmap a $ ta b $ mm (sd w [1,0]) i)

feedRelu w b = feed2 w b (\a -> maximum a 0.0)
feedSigmoid w b = feed2 w b (\a -> exp a / (exp a + 1.0)

--takes a layer as input and "applies" the layer to an input, with an output of activations
feedForward :: (Num a) => Tensor a -> [Tensor a] -> [Tensor a] -> [a -> a] -> [Tensor a]
feedForward inputs weights biases activations = foldl (\list index -> list ++ [feed (last list) (weights !! index) (biases !! index) (activations !! index)]) [inputs] [0..(pred $ length weights)]
