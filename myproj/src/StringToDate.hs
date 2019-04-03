module StringToDate where


data Date a = Cldr (Int,Int,Int) | Day Int deriving (Show, Eq, Ord, Read)

--Converts a Calendar date in the form year,month,day into a integer
getDay :: (Show a)=>Date a-> Date Int
getDay (Cldr(y,m,d)) = let m' = processMonth (Cldr(y,m,d)) ;y' = processYear (Cldr(y,m,d)) 
                       in Day(processDay y' m' d)  

processMonth :: Date a -> Int
processMonth (Cldr(_,m,_)) = ((m+9)`mod` 12)
processYear (Cldr(y,m,d)) = y - ((processMonth(Cldr(y,m,d))) `div` 10)

processDay :: Int -> Int -> Int -> Int
processDay y m d = 365*y + y `div` 4 - y `div` 100 + y `div` 400 + ((m*306 + 5) `div` 10) + (d -1)

--difference Days
diffDays :: Date Int -> Date Int -> Int
diffDays (Day a)(Day b) = a - b

--gets a date from a day number
convertToDate :: (Show a)=>Date Int -> Date a
convertToDate (Day g) = let y = convertToYear (Day g) ; d = convertToDays (Day g) y ; m = convertToMonth d ;  
                        in Cldr(dYear y m, dMonth m, dDay d m)
                      

convertToYear :: Date Int -> Int
convertToYear (Day g) = (10000*g + 14780) `div` 3652425

convertToDays :: Date Int -> Int -> Int
convertToDays (Day g) y = g - (365*y + y `div` 4 - y `div` 100 + y `div` 400)

convertToMonth :: Int -> Int
convertToMonth d = (100*d + 52) `div` 3060

dYear :: Int -> Int -> Int
dYear y m =  y + ((m + 2) `div` 12)
dDay d m = d - ((m*306 + 5) `div` 10) + 1 

dMonth :: Int -> Int
dMonth m = ((m + 2) `mod` 12) + 1



--algorithm from https://alcor.concordia.ca/~gpkatch/gdate-algorithm.html
-- function d(g)
-- y = (10000*g + 14780)/3652425
-- ddd = g - (365*y + y/4 - y/100 + y/400)
-- if (ddd < 0) then
 -- y = y - 1
 -- ddd = g - (365*y + y/4 - y/100 + y/400)
 -- endif
-- mi = (100*ddd + 52)/3060
-- mm = (mi + 2)%12 + 1
-- y = y + (mi + 2)/12
-- dd = ddd - (mi*306 + 5)/10 + 1
-- return y, mm, dd