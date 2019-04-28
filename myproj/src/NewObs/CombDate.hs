module CombDate where

-- ===================================================================================================================================================================
--  DATE  
-- ===================================================================================================================================================================	  


data Date = C (Integer,Int,Int) deriving (Show, Eq, Ord, Read)  

data Day =  Day Integer deriving (Show, Eq, Ord, Read)

instance Num Day where
    Day x + Day y = Day(x+y)
    Day x - Day y = Day(x-y)
    Day x * Day y = Day(x*y)
    abs (Day x)    | (Day x) >= 0  = (Day x)  
                   | (Day x) <  0  = -(Day x) 
 
    signum (Day x) | (Day x) >  0  = 1  
                   | (Day x) == 0  = 0  
                   | (Day x) <  0  = -1
    fromInteger x = Day x


dayToInteger :: Day  -> Integer
dayToInteger (Day x) = x

--Operations adding/subtracting days to a Date
--incrementDate :: Date -> Day -> Date

incrementDate (C(t1,t2,t3)) x = 
          toDate (getDay (C(t1,t2,t3)) + x)

--Convert Date into a String with format Year/Month/Day
--Algorithm from https://alcor.concordia.ca/~gpkatch/gdate-algorithm.html

date2String :: Date -> String
date2String (C(t1,t2,t3))= show t1 ++ "/" ++ show t2 ++ "/" ++ show t3  

--Converts a Calendar date in the form year,month,day into a integer

getDay :: Date -> Day
getDay (C(y,m,d)) =
       let m' = processMonth (C(y,m,d)) ;
           y' = processYear (C(y,m,d)) 
           in  (Day (processDay y' m' d))

  
-- getDay Inner workings acording to the algorithm found in	
-- https://alcor.concordia.ca/~gpkatch/gdate-algorithm.html	
   
processMonth :: Date -> Integer
processMonth (C(_,m,_)) = toInteger((m+9)`mod` 12)

processYear :: Date -> Integer
processYear (C(y,m,d)) = y - ((processMonth(C(y,m,d))) `div` 10)

processDay :: Integral a => Integer -> Integer -> a -> Integer
processDay y m d = 
      let d' = toInteger d 
      in 365*y + y `div` 4 - y `div` 100 + y `div` 400 + 
                                              ((m*306 + 5) `div` 10) + (d' -1)


--gets a date from a day number
toDate :: Day -> Date 
toDate g = let g' = dayToInteger g ; 
               y = convertToYear g' ;
               d = convertToDays g' y ; 
               m = convertToMonth d ;  
               in C(dYear (correctYear (dayToInteger g) y) m,
                                                           dMonth m, dDay d m)


--Day to Date Inner Workings
--Intermediate Step to calculate the date
--convertToYear :: Integer -> Integer
convertToYear g = (10000*g + 14780) `div` 3652425

--convertToDays :: Int -> Integer -> Int
convertToDays g y = 
       let y' = fromInteger y ;
           g' = g - (365*y + y `div` 4 - y `div` 100 + y `div` 400)
           in convertToDays' y' g g'

--Leap year Corrections
convertToDays' y' g  g' | g' < 0 = 
                             g - (365*(y'-1) + (y'-1) `div` 4 
                                        - (y'-1) `div` 100 + (y'-1) `div` 400)
                        |otherwise = g'

correctYear g y  | (g - (365*y + y `div` 4 - y `div` 100 + y `div` 400)) < 0 =
                                                                          (y-1)
                 | otherwise = y

--convertToMonth :: Int -> Int
convertToMonth d = (100*d + 52) `div` 3060

--Final Step to Calculate de Date
--dYear :: Integer -> Int -> Integer
dYear y m = let m' = toInteger ((m + 2) `div` 12) in y + m'

dDay d m = fromInteger (d - ((m*306 + 5) `div` 10) + 1)

--dMonth :: Int -> Int
dMonth m = fromInteger(((m + 2) `mod` 12) + 1)

-- Other OPerations with dates
--Returns the difference between two dates in num of Days
--dateDiff :: Date -> Date -> Day

dateDiff (C(t1,t2,t3)) (C(s1,s2,s3)) = 
          getDay(C(t1,t2,t3)) - getDay(C(s1,s2,s3))
