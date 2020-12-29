import Data.List

main = do
    lines' <- lines <$> readFile "day6input.txt"
    print . sum . map (length . nub) $ words $ concatMap (\x -> if x == "" then " " else x) lines'