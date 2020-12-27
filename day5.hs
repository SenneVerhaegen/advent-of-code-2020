import Data.List

main = do
    boardingPasses <- lines <$> readFile "day5input.txt"
    let seats   = map (mkSeat . splitAt 7) boardingPasses
        seatIds = sort $ map (\ (Seat _ _ seatId) -> seatId) seats
        max     = maximum seatIds
        missing = head $ missingSeats seatIds
        -- in print max
        in print missing

missingSeats seats = [ n row col | row <- [0..127], col <- [0..7], n row col > 10 && n row col <= maximum seats] \\ seats
    where n row col = (row * 8) + col

data Seat = Seat Int Int Int deriving (Show)

mkSeat :: (String, String) -> Seat
mkSeat (x, y) = 
    let row = bp (== 'F') (== 'B') 0 127 x
        col = bp (== 'L') (== 'R') 0 7 y
        seatId = (row * 8) + col
    in Seat row col seatId

bp :: (Char -> Bool) -> (Char -> Bool) -> Int -> Int -> [Char] -> Int
bp p1 p2 lowerbound upperbound [x] = if p1 x then lowerbound else upperbound
bp p1 p2 lowerbound upperbound (x:xs)
    | p1 x = bp p1 p2 lowerbound (upperbound - diff lowerbound upperbound) xs -- Lower half
    | p2 x = bp p1 p2 (lowerbound + diff lowerbound upperbound) upperbound xs -- Upper half
    where diff lower upper = ((upper - lower) `div` 2) + 1