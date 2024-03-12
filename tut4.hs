module Tut4 where
import qualified Data.Map as Map 
import Control.Exception (Exception, throw)
data DimensionalityMismatch = DimensionalityMismatch deriving Show
instance Exception DimensionalityMismatch

mulv :: Num a => a -> [a] -> [a]
mulv _ [] = []
mulv n (x:xs) = (n*x) : mulv n xs

vecadd :: Num a => [a] -> [a] -> [a]
vecadd [] [] = []
vecadd (x:xs) (y:ys) = case length xs /= length ys of
    True -> throw DimensionalityMismatch
    False -> x + y : vecadd xs ys

vecminus :: Num a => [a] -> [a] -> [a]
vecminus [] [] = []
vecminus (x:xs) (y:ys) = case length xs /= length ys of
    True -> throw DimensionalityMismatch
    False -> x - y : vecminus xs ys

dot :: Num a => [a] -> [a] -> a
dot [] [] = 0
dot (x:xs) (y:ys) = case length xs /= length ys of
    True -> throw DimensionalityMismatch
    False -> x * y + dot xs ys

normL2 :: Floating a => [a] -> a
normL2 [] = 0
normL2 x = sqrt (sum (map (\xi -> xi^2) x))

cosine :: Floating a => [a] -> [a] -> a
cosine x y = case length x /= length y of
    True -> throw DimensionalityMismatch
    False -> dot x y / (normL2 x * normL2 y)

euclidean :: Floating a => [a] -> [a] -> a
euclidean x y = sqrt (sum (map (\(xi, yi) -> (xi - yi)^2) (zip x y)))

parseLine :: String -> (String, [Double])
parseLine line =
    let (key, values) = span (/= ' ') line
        vectorValues = map read $ words values
    in (key, vectorValues)

readVectorSpace :: FilePath -> IO (Map.Map String [Double])
readVectorSpace filePath = do
    contents <- readFile filePath
    let pairs = map parseLine (lines contents)
    return $ Map.fromList pairs

main :: IO ()
main = do
    vecSpace <- readVectorSpace "sample_vecspace_an.txt"
    print vecSpace