import System.IO

main = do
    contents <- readFile "day3input.txt"
    let fileLines = lines contents
    print . countTrees . trimLines $ expandColumns fileLines

expandColumns :: [String] -> [String]
expandColumns rows = map (filter (/= ' ') . unwords . replicate (length rows * 3)) rows

trimLines :: [String] -> [String]
trimLines rows = zipWith (\ row rowCounter -> drop (3 * rowCounter) row) rows [0 .. length rows + 1]

countTrees :: [String] -> Int
countTrees rows = length $ filter (\line -> head line == '#') rows