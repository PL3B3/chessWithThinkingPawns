module Layer (
    Network(..),
    Layer(..),
    Weights(..),
    makeNetwork,
    calcNetwork,
    layersApply,
    weightsToVals,
    f2r
) where

import MatrixMaths

--Layer in a neural network
type Layer = [Double] 

--The weights have a length equal to (layer before) * (layer after)
--Each segment of length (layer before) of the weights is multiplied by their corresponding element of layer before
type Weights = [Double]

--Just a way to hold data together
data Network = Network {
    weights :: [Weights],
    layers :: [Layer],
    afuncs :: [[Double] -> Double -> Double]
}

makeNetwork :: [Int] -> Network
makeNetwork dims = Network {weights=weights, layers=layers, afuncs=(repeat (\a b -> sum a + b))}
    where len = length dims
          layers = map (\a -> take a $ repeat 1.0) dims
          weights = map (\x -> take x (repeat 1.0)) (mult dims)
          mult ls@(x:y:ys)
            | ls == [] = []
            | ys == [] = [x * y]
            | otherwise = (x * y):mult (y:ys) 

calcNetwork :: Network -> Network
calcNetwork net = Network {weights=w, layers=(layersApply w l a), afuncs=a} 
    where w = weights net
          l = layers net
          a = afuncs net

layersApply :: [Weights] -> [Layer] -> [([Double] -> Double -> Double)] -> [Layer]
layersApply weights layers activations = reverse $ foldl (\l w -> (zipWith (activations !! (length l)) (weightsToVals w (layers !! (length l))) (layers !! (succ $ length l))):l) [] weights

weightsToVals :: Weights -> Layer -> [[Double]] 
weightsToVals weights layer
    | weights == [] = []
    | otherwise = [(zipWith (*) stuff layer)] ++ (weightsToVals rest layer)
        where len = length layer 
              stuff = take len weights
              rest = drop len weights

-- converts float to nearest int if within threshold
f2r f
    | closeToFloor f = (floor f, 1)
    | closeToFloor fInverse = (1, floor fInverse)
    | closeToCeil f = (ceiling f, 1)
    | closeToCeil fInverse = (1, ceiling fInverse)
    | floor f == 0 = flipped $ incorp 0 (f2r fInverseTail)
    | otherwise = incorp (floor f) (f2r fractionPart) 
        where thresh = 0.00001
              closeToFloor n = (abs $ (fromIntegral $ floor n) - n) < thresh 
              closeToCeil n = (abs $ (fromIntegral $ ceiling n) - n) < thresh
              incorp int frac@(a,b) = (int * b + a, b)
              flipped (a,b) = (b,a)
              fractionPart = f - (fromIntegral $ floor f) 
              fInverse = 1.0 / f
              fInverseTail = 1.0 / fractionPart
