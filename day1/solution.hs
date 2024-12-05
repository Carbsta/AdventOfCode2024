import Data.ByteString.Char8 (split)
import Data.List (sort)

readInt :: String -> Int
readInt = read

splitOnPos :: [a] -> ([a], [a])
splitOnPos (x : xs) = (x : evens, odds) where (odds, evens) = splitOnPos xs
splitOnPos [] = ([], [])

readLists :: String -> ([Int], [Int])
readLists = splitOnPos . map readInt . words

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print ("Part 1, list distance: " ++ (show . listDist . readLists $ contents))
  print ("Part 2, list similarity: " ++ (show . listSim . readLists $ contents))

dist :: Int -> Int -> Int
dist x y = abs (x - y)

listDist :: ([Int], [Int]) -> Int
listDist (xs, ys) = sum (zipWith dist (sort xs) (sort ys))

count :: (Eq a) => a -> [a] -> Int
count x xs = length (filter (== x) xs)

itemSim :: [Int] -> Int -> Int
itemSim xs x = x * count x xs

listSim :: ([Int], [Int]) -> Int
listSim (xs, ys) = sum (map (itemSim ys) xs)