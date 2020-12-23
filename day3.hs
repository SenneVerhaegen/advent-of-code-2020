import System.IO

main = do
    fileLines <- lines <$> readFile "day3input.txt"
    print . countTrees . trimLines $ expandColumns fileLines

expandColumns :: [String] -> [String]
expandColumns rows = map (filter (/= ' ') . unwords . replicate (length rows * 3)) rows

trimLines :: [String] -> [String]
trimLines rows = zipWith (\ row rowCounter -> drop (7 * rowCounter) row) rows [0 .. length rows]

countTrees :: [String] -> Int
countTrees rows = length $ filter (\line -> head line == '#') rows