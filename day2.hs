import System.IO
import System.Environment
import Data.Char

main = do
    contents <- readFile "day2input.txt"
    let fileLines = lines contents
    print . length . filter (== True) $ map (\line ->
        let parts       = words line
            interval    = mkInterval $ head parts
            c           = head $ parts !! 1
            pwd         = parts !! 2
        in validPassword interval c pwd) fileLines

data Interval = Interval { lowerBound :: Int, upperBound :: Int } deriving (Show)

type Password = String

mkInterval :: String -> Interval
mkInterval str = 
    let list = words $ map (\x -> if x == '-' then ' ' else x) str
    in Interval (read (head list) :: Int) (read (list !! 1) :: Int)

validPassword :: Interval -> Char -> Password -> Bool
validPassword interval c pwd = 
    let occurences = length $ filter (== c) pwd
    in lowerBound interval <= occurences && occurences <= upperBound interval
