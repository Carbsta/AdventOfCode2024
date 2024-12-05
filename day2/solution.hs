import Data.Sequence (chunksOf)

readInt :: String -> Int
readInt = read

readReports :: String -> [[Int]]
readReports s = map (map readInt . words) (lines s)

(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) p1 p2 x = p1 x || p2 x

(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) p1 p2 x = p1 x && p2 x

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs xs = zip xs (tail xs)

allPairs :: (Int -> Int -> Bool) -> [(Int, Int)] -> Bool
allPairs _ [] = True
allPairs f ((x, y) : xs) = f x y && allPairs f xs

isDiffSafe :: Int -> Int -> Bool
isDiffSafe x y = diff < 4 && diff > 0
  where
    diff = abs (x - y)

arePairsSafe :: [(Int, Int)] -> Bool
arePairsSafe = (allPairs (>) ||| allPairs (<)) &&& allPairs isDiffSafe

isReportSafe :: [Int] -> Bool
isReportSafe = arePairsSafe . pairs

countSafeReports :: [[Int]] -> Int
countSafeReports = length . filter id . map isReportSafe

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print ("Part 1, number of safe reports: " ++ show (countSafeReports $ readReports contents))