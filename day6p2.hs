import Data.List

main = do
    lines' <- lines <$> readFile "day6input.txt"
    print . sum . map (length . myf) $ fst $ group' ([], map (: []) lines') 

myf :: [String] -> String
myf = foldr intersect ['a'..'z']

group' :: ([[String]], [[String]]) -> ([[String]], [[String]])
group' (grouped, []) = (grouped, [])
group' (grouped, ungrouped) = 
    let tuple       = break (== [""]) ungrouped
        newGrouped  = (foldr (++) [] (fst tuple)) : grouped
    in group' (newGrouped, drop 1 $ snd tuple)