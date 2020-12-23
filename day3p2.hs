import System.IO

-- Solution is incorrect for the last tuple (1,2)

main = do
    fileLines <- lines <$> readFile "day3input.txt"
    print $ map (\(stepsRight, stepsDown) -> 
        countTrees $ trimLines (expandColumns fileLines stepsRight) stepsRight stepsDown) [(1,1),(3,1),(5,1),(7,1),(1,2)]

expandColumns :: [String] -> Int -> [String]
expandColumns rows stepsRight = map (filter (/= ' ') . unwords . replicate (length rows * stepsRight)) rows

trimLines :: [String] -> Int -> Int -> [String]
trimLines rows stepsRight stepsDown = 
    let rows' = 
            if stepsDown > 1 then head rows : fmap fst (filter (\(_, y) -> y `mod` stepsDown /= 0) $ zip rows [0 .. length rows])
            else rows
    in zipWith (\ row rowCounter -> drop (stepsRight * rowCounter) row) rows' [0 .. length rows']

countTrees :: [String] -> Int
countTrees rows = length $ filter (\line -> head line == '#') rows