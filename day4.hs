import System.IO
import Data.List

main = do
    lines' <- lines <$> readFile "day4input.txt"
    print . length . filter (== True) $ map (isValid . words) (fst $ groupFields ([], lines'))

groupFields :: ([String], [String]) -> ([String], [String])
groupFields (grouped, []) = (grouped, [])
groupFields (grouped, ungrouped) =
    let tuple = break (== "") ungrouped
    in groupFields (concatMap (++ " ") (fst tuple) : grouped, drop 1 (snd tuple))

isValid :: [String] -> Bool
isValid passport = case length $ nub passport of 
    8 -> all delegate passport
    7 -> "cid" `notElem` map getField passport && all delegate passport
    _ -> False

getField :: String -> String
getField = takeWhile (/= ':')

delegate :: String -> Bool
delegate field = 
    let tuple = break (== ':') field
        value = drop 1 $ snd tuple
    in case fst tuple of
        "byr" -> valBirthYear value
        "iyr" -> valIssueYear value
        "eyr" -> valExpirationYear value
        "hgt" -> valHeight value
        "hcl" -> valHairColor value
        "ecl" -> valEyeColor value
        "pid" -> valPassportID value
        "cid" -> True
        _     -> False

valBirthYear :: String -> Bool
valBirthYear value =
    let year = (read value :: Int)
    in year >= 1920 && year <= 2020

valIssueYear :: String -> Bool
valIssueYear value =
    let year = (read value :: Int)
    in year >= 2010 && year <= 2020

valExpirationYear :: String -> Bool
valExpirationYear value = 
    let year = (read value :: Int)
    in year >= 2020 && year <= 2030

valHeight :: String -> Bool
valHeight value = 
    let tuple = splitAt (length value - 2) value
        height = read (fst tuple) :: Int
    in case snd tuple of 
        "cm" -> height >= 150 && h <= 193
        "in" -> height >= 59 && h <= 76
        _    -> False

-- Should use regex
valHairColor :: String -> Bool
valHairColor value = True

valEyeColor :: String -> Bool
valEyeColor value = value `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

-- Should use regex
valPassportID :: String -> Bool
valPassportID value = True
