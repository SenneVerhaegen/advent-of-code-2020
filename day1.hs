-- Finds n entries in a list that sum to a target value
-- findEntries :: Int -> Int -> [Int] -> Maybe [Int]
-- findEntries n target (x:xs) = findEntriesInternal n target x xs

-- findEntriesInternal :: Int -> Int -> Int -> [Int] -> Maybe [Int]
-- findEntriesInternal 2 target part (x:xs)
--     | length (x:xs) == 1            = if part + x == target then Just [part, x] else Nothing
--     | length (x:xs) >= 2            = if target - part `elem` (x:xs) then Just [part, target - part] else findEntriesInternal 2 target x xs
-- findEntriesInternal n target part (x:xs)
    -- | length (x:xs) > 2             = case findEntriesInternal (n - 1) (target - part) x xs of
    --         Nothing -> findEntriesInternal n (target - x) (head xs) (tail xs)
    --         result -> cc part result
    -- | otherwise = Nothing


-- Prepends an a to an optional list of a's
-- cc :: a -> Maybe [a] -> Maybe [a]
-- cc x list = Just . (:) x $ concat list

problem1 list = head [x*y | x <- list, y <- list, x+y == 2020]
problem2 list = head [x*y*z | x <- list, y <- list, z <- list, x+y+z == 2020]