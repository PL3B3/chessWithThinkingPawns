module LinearAlgebra (
    Tensor(Tensor),
    makeTensor,
    shapeOf,
    infoOf,
    splitInto,
    get_row,
    get_column,
    mtranspose,
    drawMatrix,
    vm,
    pp,
    pl,
    sd,
    mm,
    te,
    ta,
    tm,
    tc2,
    valueAt,
    toIndex,
    toCoordinate
) where

import Debug.Trace
import Data.List

data Tensor a = Tensor [Int] [a] deriving (Show, Eq)

instance Functor Tensor where
    fmap function tensor@(Tensor shape info) = Tensor shape (map function info)

makeTensor :: [Int] -> [a] -> Tensor a
makeTensor shape info = Tensor shape info

customConcat a
    | a == [] = []
    | tail a == [] = head a
    | otherwise = (head a) ++ customConcat (tail a)

drawMatrix :: (Show a) => Tensor a -> String
drawMatrix t1@(Tensor shape info) = foldl (\string row ->  string ++ customConcat (map (\a -> show a ++ (customConcat $ take (bigL - (ls a)) $ repeat " ")) row) ++ "\n") "" rows
    where rows = splitInto info (shape !! 1)
          ls = length . show
          bigL = succ $ maximum $ map (\a -> ls a) info

shapeOf :: Tensor a -> [Int]
shapeOf (Tensor shape info) = shape

infoOf :: Tensor a -> [a]
infoOf (Tensor shape info) = info

--splits Into chunks of size "size"
splitInto :: [a] -> Int -> [[a]]
splitInto list size
    | length list == 0 = []
    | otherwise = [take size list] ++ splitInto (drop size list) size

vm :: (Num a) => [a] -> [a] -> a
vm v1 v2 = sum $ zipWith (*) v1 v2

pp :: [Int] -> [Int]
pp l = [0..(pred $ product l)]

pl :: [a] -> [Int]
pl l = [0..(pred $ length l)]

sd :: Tensor a -> [Int] -> Tensor a
sd t@(Tensor shape info) fd = Tensor newShape (map (\a -> valueAt t $ (flip $ toCoordinate a newShape)) (pp shape))
    where newShape = flip shape
          flip l = map (\b -> l !! b) fd

valueAt :: Tensor a -> [Int] -> a
valueAt (Tensor shape info) coordinate = info !! (toIndex coordinate shape)

toIndex :: [Int] -> [Int] -> Int
toIndex coordinate dimensions = vm coordinate sizes
    where sizes = (map (\a -> product $ drop a dimensions) [1..(pred $ length dimensions)]) ++ [1]

toCoordinate :: Int -> [Int] -> [Int]
toCoordinate index dimensions
    | length dimensions == 0 = []
    | otherwise = [div index dTail] ++ toCoordinate (mod index dTail) (tail dimensions)
        where dTail = product $ tail dimensions

--apply to matrices only
get_row :: Tensor a -> Int -> Tensor a
get_row t@(Tensor shape info) index = Tensor [1,(shape !! 1)] ((splitInto info (shape !! 1)) !! index)

get_column tensor index = mtranspose (get_row (mtranspose tensor) index)

mtranspose tensor = sd tensor [1,0]

--simple 2d matrix multiplication. A, B => AB
mm :: (Num a) => Tensor a -> Tensor a -> Tensor a
mm (Tensor shape1 info1) t2@(Tensor shape2 info2) = Tensor [dim1, dim4] newInfo
    where (dim1, dim2) = (shape1 !! 0, shape1 !! 1)
          (dim3, dim4) = (shape2 !! 0, shape2 !! 1)
          columns = (splitInto (infoOf $ sd t2 [1,0]) dim3)
          newInfo = foldl (\newInfo row -> newInfo ++ map (vm row) columns) [] (splitInto info1 dim2)


--apply a function elementwise to the corresponding elements of two Tensors
te :: Tensor a -> Tensor a -> (a -> a -> a) -> Tensor a
te t1@(Tensor s1 i1) t2@(Tensor s2 i2) func = Tensor s1 (zipWith (func) i1 i2)

--elementwise tensor addition
ta :: (Num a) => Tensor a -> Tensor a -> Tensor a
ta t1 t2 = te t1 t2 (+)

--elementwise tensor multiplication: Hadamard product
tm :: (Num a) => Tensor a -> Tensor a -> Tensor a
tm t1 t2 = te t1 t2 (*)

--tensor contraction. product of all the elements
--takes in tensors t1 and t2 and an "overlap," the number of dimensions shared between the two. The resultant tensor is t3. Say t1 is dimensions (a,b,c) and t2 is dimensions (c,b,d). We have overlap 2. This means that t3 has dimensions (a,d) because we "contract" the overlapping dimensions, aka do this: t3(a0,d0) = sum(b,c) (t1(a0,b,c) * t2(c,b,d0))
--frontCoordinates are the list of every t1(a0,b,c), and backCoordinates are the list of every t2(c,b,d0). I then multiply every value at corresponding frontCoordinates/backCoordinates, such as (a0, 1 ,2) and (2, 1, d0), and the sum of that is the value of t3 at (a0, d0). I do this for every combination of a and d and map those results to a new tensor with shape (shape1 with the overlap dimensions dropped, + shape2 with overlap dimensions dropped)
tc2 :: (Num a, Show a) => Tensor a -> Tensor a -> Int -> Tensor a
tc2 t1@(Tensor shape1 info1) t2@(Tensor shape2 info2) overlap = Tensor newShape newInfo
        where (fs1, bs1) = splitAt ((length shape1) - overlap) shape1
              (fs2, bs2) = splitAt overlap shape2
              newShape = fs1 ++ bs2
              getValue newCoordinates = (sum $ (zipWith (*) (map (valueAt t1 $) (frontCoordinates newCoordinates)) (map (valueAt t2 $) (backCoordinates newCoordinates))))
              frontCoordinates newCoordinates = map (\a -> (take halfLength newCoordinates) ++ toCoordinate a bs1) (pp bs1)
              backCoordinates newCoordinates = map (\a -> (reverse $ toCoordinate a fs2) ++ (drop halfLength newCoordinates)) (pp fs2)
              newInfo = map (\newInfoIndex -> getValue $ toCoordinate newInfoIndex newShape) (pp newShape)
              halfLength = div (length newShape) 2
