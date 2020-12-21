import System.IO
import System.Environment
import Data.Char

main = do
    contents <- readFile "day2input.txt"
    let fileLines = lines contents
    print . length . filter (== True) $ map (\line ->
        let parts       = words line
            tuple       = mkTuple $ head parts
            c           = head $ parts !! 1
            pwd         = parts !! 2
        in validPassword tuple c pwd) fileLines

type Password = String

mkTuple :: String -> (Int, Int)
mkTuple str = 
    let list = words $ map (\x -> if x == '-' then ' ' else x) str
    in (read (head list) :: Int, read (list !! 1) :: Int)

validPassword :: (Int, Int) -> Char -> Password -> Bool
validPassword positions c pwd = 
    xor (c == (pwd !! (fst positions - 1))) (c == (pwd !! (snd positions - 1)))

xor :: Bool -> Bool -> Bool
xor a b = (a && not b) || (not a && b)