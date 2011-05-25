
data Month = January | February | March | April | May | June | July | August | September | October | November | December deriving (Enum, Show, Eq)

-- takes month and year, calculates days in that month
daysInMonth :: Month -> Int -> Int
daysInMonth February year   = if (year `rem` 4 == 0) then 29 else 28
daysInMonth month _ = if month `elem` [September, April, June, November]
                      then 30
                      else 31

-- create lazy list of days in each month in range
daysList = [daysInMonth month year | year<-[1901..2000], month<-enumFrom January]

-- go through list of days in month summing days.
-- wherever the days div perfectly by 7 it should be sunday! 
dayfold = length $ filter  (\n -> n `rem` 7 == 0) (scanl (+) 0 daysList)

main = print $ dayfold

