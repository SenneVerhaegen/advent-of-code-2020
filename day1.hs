-- Finds n entries in a list that sum to a target value
findEntries :: Int -> [Int] -> Maybe [Int]
findEntries target (x:xs) = findEntriesInternal target x xs

findEntriesInternal :: Int -> Int -> [Int] -> Maybe [Int]
findEntriesInternal target part (x:xs)
    | length (x:xs) == 1 = if part + x == target then Just [part, x] else Nothing
    | otherwise = if target - part `elem` (x:xs) then Just [part, target - part] else findEntriesInternal target x xs

resultProblem1 :: Maybe [Int] -> Int
resultProblem1 (Just entries) = product entries
resultProblem1 Nothing = error "No entries found"