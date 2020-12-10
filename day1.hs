-- Finds n entries in a list that sum to a target value
findEntries :: Int -> Int -> [Int] -> Maybe [Int]
findEntries target 0 (x:xs)    = findEntries target x xs
findEntries target part (x:xs)
    | length (x:xs) == 1 = if part + x == target then Just [part, x] else Nothing
    | otherwise = if target - part `elem` (x:xs) then Just [part, target - part] else findEntries target x xs

resultProblem1 :: Maybe [Int] -> Int
resultProblem1 (Just entries) = product entries
resultProblem1 Nothing = error "No entries found"