module StringToDate where

import Data.Time.Clock
import Data.Time.Calendar

date :: IO (Integer,Int,Int) -- :: (year,month,day)
date = getCurrentTime >>= return . toGregorian . utctDay


data Date = C (Integer,Int,Int) deriving (Show, Eq, Ord, Read)   -- | Day Int

--Create your own date
mkDate ::  Date
mkDate = (C(2019,4,6))

time0 :: Date
time0 = C(0,0,0)

--Returns the day the contract expires
horizon :: Date -> Int
horizon (C(t1,t2,t3)) =  getDay (C(t1,t2,t3))

--Returns the difference between two dates in Int
dayDiff :: Date -> Date -> Int
dayDiff (C(t1,t2,t3)) (C(s1,s2,s3)) = getDay(C(t1,t2,t3)) - getDay(C(s1,s2,s3))

--returns the further/nearer date from now ("day 0") in days
maxDate :: Date -> Date -> Date
maxDate (C(t1,t2,t3))(C(s1,s2,s3)) = max (C(t1,t2,t3)) (C(s1,s2,s3))

minDate :: Date -> Date -> Date
minDate (C(t1,t2,t3))(C(s1,s2,s3)) = min (C(t1,t2,t3)) (C(s1,s2,s3))

--Operations adding/subtracting days to a Date
incrementDate :: Date -> Int -> Date
incrementDate (C(t1,t2,t3)) x = toDate(getDay(C(t1,t2,t3)) + x)

date2String :: Date -> String
date2String (C(t1,t2,t3))= show t1 ++ "/" ++ show t2 ++ "/" ++ show t3  






--IO attempt
 --comparing dates, Use of IO
-- atDate (C(a,b,c)) (C(x,y,z))
     -- |(a,b,c) == (x,y,z) = True 
     -- | otherwise = False

-- atDateMixedIO (C(a,b,c)) (x,y,z)
     -- |(a,b,c) <= (x,y,z) = True 
     -- | otherwise = False

-- atDateIO (a,b,c)(x,y,z)
     -- |(a,b,c) == (x,y,z) = True 
     -- | otherwise = False


-- dateIO :: Date -> IO Bool
-- dateIO a = do
  -- (x,y,z) <- date
  -- return (atDateMixedIO a (x,y,z))
  




  
  
  
  
--Converts a Calendar date in the form year,month,day into a integer
--------------------------------------------------------------------------------------------------------
getDay :: Date -> Int
getDay (C(y,m,d)) = let m' = processMonth (C(y,m,d)) ;y' = processYear (C(y,m,d)) 
                       in (processDay y' m' d)  

processMonth :: Date -> Int
processMonth (C(_,m,_)) = ((m+9)`mod` 12)
processYear :: Date -> Integer
processYear (C(y,m,d)) = y - ((toInteger(processMonth(C(y,m,d)))) `div` 10)

processDay :: Integer -> Int -> Int -> Int
processDay y m d = let y' = fromInteger y 
       in 365*y' + y' `div` 4 - y' `div` 100 + y' `div` 400 + ((m*306 + 5) `div` 10) + (d -1)


--gets a date from a day number
toDate :: Int -> Date 
toDate g = let y = convertToYear g ; d = convertToDays g y ; m = convertToMonth d ;  
                        in C(dYear y m, dMonth m, dDay d m)
                      

--Day to Date Inner Workings
--Intermediate Step to calculate the date
convertToYear :: Int -> Integer
convertToYear g = toInteger((10000*g + 14780) `div` 3652425)

convertToDays :: Int -> Integer -> Int
convertToDays g y = let y' = fromInteger y in  g - (365*y' + y' `div` 4 - y' `div` 100 + y' `div` 400)

convertToMonth :: Int -> Int
convertToMonth d = (100*d + 52) `div` 3060

--Final Step to Calculate de Date
dYear :: Integer -> Int -> Integer
dYear y m = let m' = toInteger ((m + 2) `div` 12) in y + m'

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