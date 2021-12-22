module Stats (
  avg,
  variance,
  std_dev,
  median
) where

import System.IO
import Data.List
import System.Directory

xp :: (Num a, Fractional a) => a -> Int -> a
xp base power = product ((take power (repeat base)) ++ [1.0])

avg :: [Double] -> Double 
avg x = (sum x) / (fromIntegral  (length x))

variance :: [Double] -> Double
variance x = sum [xp (a - (avg x)) 2| a <- x]

std_dev :: [Double] -> Double
std_dev x = sqrt $ variance x

blobSort :: Ord a => [a] -> [a]
blobSort [] = []
blobSort (x:xs) = (blobSort under) ++ [x] ++ (blobSort over)
  where
    under = filter (< x) xs
    over = filter (>= x) xs

median :: [Double] -> Double
median [] = 0
median x
  | odd (length xSort) = xSort !! midDex
  | even (length xSort) = avg [xSort !! midDex, xSort !! (midDex - 1)]
  where xSort = blobSort x
        midDex = floor ((fromIntegral (length xSort)) / 2)