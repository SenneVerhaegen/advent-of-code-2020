-- Finds n entries in a list that sum to a target value
findEntries :: Int -> Int -> [Int] -> Maybe [Int]
findEntries n target (x:xs) = findEntriesInternal n target x xs

findEntriesInternal :: Int -> Int -> Int -> [Int] -> Maybe [Int]
findEntriesInternal n target part (x:xs)
    | length (x:xs) == 1            = if part + x == target then Just [part, x] else Nothing
    | length (x:xs) == 2            = Just [n, part]
    | length (x:xs) > 2 && n == 2   = if target - part `elem` (x:xs) then Just [part, target - part] else findEntriesInternal n target x xs
    | otherwise                     = cc part $ findEntriesInternal (n - 1) (target - x) x xs

resultProblem1 :: Maybe [Int] -> Int
resultProblem1 (Just entries) = product entries
resultProblem1 Nothing = error "No entries found"

cc :: Int -> Maybe [Int] -> Maybe [Int]
cc x list = Just . (:) x $ concat list