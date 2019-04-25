{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NewObs where

import Numeric

showFullPrecision x = showFFloat Nothing x ""

-- ====================================================================================================================================================================
--  OBSERVABLES
-- ====================================================================================================================================================================

data Obs a = O (String, a)
  deriving (Show, Read)

--Observation to string
kindOfObs :: Show a => Obs a -> String
kindOfObs (O (o1, o2)) = o1 ++ " " ++ (show o2) 

--return name of observation
nameObs :: Obs a-> String
nameObs (O (o1, _)) = o1

--real-numerical value associated to O
valObs :: Obs a -> a
valObs (O(_, o2)) = o2

--create observation
createObs :: String -> a -> Obs a
createObs o1 o2 = O (o1, o2)
--
--Opertting with Obs
lift2 :: (a -> b -> c) -> Obs a -> Obs b -> Obs c
lift2 (+) o1 o2 = O(nameObs o2 ,(valObs o1) + (valObs o2))


-- ===================================================================================================================================================================
--  DATE  
-- ===================================================================================================================================================================	  


data Date = C (Integer,Int,Int) deriving (Show, Eq, Ord, Read)   -- | Day Int


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

--Create your own date

time0 = (C(2019,4,26))

mkDate d = incrementDate d (Day 1)


--Returns the difference between two dates in num of Days
--dateDiff :: Date -> Date -> Day
dateDiff (C(t1,t2,t3)) (C(s1,s2,s3)) = getDay(C(t1,t2,t3)) - getDay(C(s1,s2,s3))

--Operations adding/subtracting days to a Date
--incrementDate :: Date -> Day -> Date
incrementDate (C(t1,t2,t3)) x = toDate(getDay(C(t1,t2,t3)) + x)

--Subtract, although it could be done 
decrementDate (C(t1,t2,t3)) x = toDate(getDay(C(t1,t2,t3)) - x)

--Convert Date into a String with format Year/Month/Day
date2String :: Date -> String
date2String (C(t1,t2,t3))= show t1 ++ "/" ++ show t2 ++ "/" ++ show t3  

--Converts a Calendar date in the form year,month,day into a integer

getDay :: Date -> Day
getDay (C(y,m,d)) = let m' = processMonth (C(y,m,d)) ;y' = processYear (C(y,m,d)) 
                       in  (Day (processDay y' m' d))

processMonth :: Date -> Integer
processMonth (C(_,m,_)) = toInteger((m+9)`mod` 12)

processYear :: Date -> Integer
processYear (C(y,m,d)) = y - ((processMonth(C(y,m,d))) `div` 10)

processDay :: Integral a => Integer -> Integer -> a -> Integer
processDay y m d = let d' = toInteger d 
       in 365*y + y `div` 4 - y `div` 100 + y `div` 400 + ((m*306 + 5) `div` 10) + (d' -1)


--gets a date from a day number
toDate :: Day -> Date 
toDate g = let g' = dayToInteger g ; y = convertToYear g' ; d = convertToDays g' y ; m = convertToMonth d ;  
                        in C(dYear (correctYear (dayToInteger g) y) m, dMonth m, dDay d m)
                      

--Day to Date Inner Workings
--Intermediate Step to calculate the date
--convertToYear :: Integer -> Integer
convertToYear g = (10000*g + 14780) `div` 3652425

--convertToDays :: Int -> Integer -> Int
convertToDays g y = let y' = fromInteger y ; g' = g - (365*y + y `div` 4 - y `div` 100 + y `div` 400)
                    in convertToDays' y' g g'

--Leap year Corrections
convertToDays' y' g  g' | g' < 0 = g - (365*(y'-1) + (y'-1) `div` 4 - (y'-1) `div` 100 + (y'-1) `div` 400)
                        |otherwise = g'

--convertToMonth :: Int -> Int
convertToMonth d = (100*d + 52) `div` 3060

--Final Step to Calculate de Date
--dYear :: Integer -> Int -> Integer
dYear y m = let m' = toInteger ((m + 2) `div` 12) in y + m'

dDay d m = fromInteger (d - ((m*306 + 5) `div` 10) + 1)

--dMonth :: Int -> Int
dMonth m = fromInteger(((m + 2) `mod` 12) + 1)

--Leap Year Corrections
correctYear g y  | (convertToDays g y) < 0 = y
                 | otherwise = (y-1)
-- =====================================================================================================================================================================
-- COMBINATORS
-- =====================================================================================================================================================================

--Notational conventions from paper
--c, d, u : Contract
--      o : Observable
--   t, s : Date, time
--      k : Currency
--      x : Dimensionless real value
--      p : Value process
--      v : Random variable


data Currency = USD | GBP | EUR | RMB | JPY | CHF | Rate String deriving (Eq, Show, Read)

--Representation of a contract
data Contract =
    Zero
  | One  Currency
  | Give Contract
  | And  Contract Contract
  | Or   Contract Contract
  | Cond    (Obs Bool) Contract Contract   
  | Scale   (Obs Double) Contract          
  | When    (Obs Bool) Contract
  | Anytime (Obs Bool) Contract            
  | Until   (Obs Bool) Contract           
  deriving (Show, Read)


--Primitives for Defining Contracts
zero :: Contract
zero = Zero                                  -- A contract with no rights nor obligations

one :: Currency -> Contract                  -- A contract that pays the buyer 1 unit of the indicated currency
one = One

give :: Contract -> Contract                 -- give reverses the rights and obligations in the contract(for every buyer there is a seller)
give = Give

cAnd :: Contract -> Contract -> Contract     --
cAnd = And

cOr :: Contract -> Contract -> Contract
cOr = Or

scale :: Obs Double  -> Contract -> Contract
scale = Scale

cond :: Obs Bool -> Contract -> Contract -> Contract
cond = Cond

cWhen :: Obs Bool-> Contract -> Contract
cWhen = When

anytime :: Obs Bool -> Contract -> Contract
anytime = Anytime

cUntil :: Obs Bool -> Contract -> Contract
cUntil = Until

--Ties a constant to an Observable 
konst :: Obs a -> b -> Obs b
konst (O(o1,_)) x = (O(o1, x))

---Used in when, checks if the contract must be activated in the current date
at :: Date -> Obs Bool
at t 
           |(mkDate time0)  >= t = O( date2String t, True) 
           |otherwise    = O( date2String t , False)
--
--Similar to at, check an interval during which
between :: Date -> Date -> Obs Bool
between t1 t2 = konst (O( ((date2String t1) ++ " and " ++ (date2String t2)), (mkDate time0) )) ( (mkDate time0) >= t1 && (mkDate time0) <= t2)

--
-- Checking Equivalence of Obs For Conditonal
checkObs o1 o2 = O((nameObs o1 ++ " is " ++ show (valObs o1)), valObs o1 == valObs o2)

--Forward (fwd) Contract Defintion
--fwd :: Obs a -> Currency -> Contract
fwd t q k =  cWhen (at t) (scale ((konst q)(valObs q)) (one k)) 

swap ::  Contract -> Contract -> Contract
swap c1 c2 =   c1 `cAnd` (give c2)

--european :: Obs Date -> Contract -> Contract
european t c = cWhen (at t) (c `cOr` zero)

--american :: Date -> Date -> Contract -> Contract
american t1 t2 c = anytime (between t1 t2) (c `cOr` zero)


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Printing
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
--Naive Print, represent all the things!!
indent :: Int -> String -> String
indent i str  = "\n" ++ (replicate i ' ') ++ str

--Prints RECEIVABLE contracts
rPrint :: Contract -> String
rPrint c = case c of
    Zero -> indent 1 "Contract with no obligations, no rights."
    One k->  " " ++ show k 
    Give u ->  "  PAY " ++ pPrint u
    And u1 u2-> rPrint u1 ++ indent 2 "AND" ++ indent 1 ( rPrint u2 )
    Or u1 u2 -> indent 2 "OPTION " ++ rPrint u1 ++ indent 1 "OR" ++ indent 2 "OPTION " ++ rPrint u2
    Cond (O(o1,o2)) u1 u2 -> indent 1 "IF "++ o1 ++ indent 2 (rPrint u1) ++ indent 1 "OTHERWISE" ++ indent 2(rPrint u2)
    Scale (O(o1,o2)) u -> "RECEIVE " ++ o1 ++ rPrint u ++ " " ++ show o2 ++ " "
    When (O(o1,o2)) u1-> indent 0 "On the " ++ o1 ++ indent 2 (rPrint u1) ++ "\n"  
    Anytime (O(o1,o2)) u -> indent 0 "Contract executable between " ++ o1 ++ indent 2 (rPrint u)
    Until (O(o1,o2)) u -> indent 0 "Until " ++ o1 ++ " " ++ rPrint u ++ "\n"

--Same as print but for PAYABLE contracts
pPrint :: Contract -> String
pPrint u = case u of 
    Zero -> indent 1 "Contract with no obligations, no rights."
    One k->  " " ++ show k 
    Give u -> "  PAY " ++ pPrint u  
    And u1 u2-> pPrint u1 ++ indent 2 "AND" ++ indent 1 ( pPrint u2 )
    Or u1 u2 -> indent 2 "OPTION " ++ pPrint u1 ++ indent 1 "OR" ++ indent 2 "OPTION " ++ pPrint u2
    Cond (O(o1,o2)) u1 u2 -> indent 1 "IF "++ o1 ++ indent 2 (pPrint u1) ++ indent 2 "OTHERWISE" ++ indent 2 (pPrint u2)
    Scale (O(o1,o2)) u ->  o1 ++ pPrint u ++ " " ++ show o2 ++ " "
    When (O(o1,o2)) u1-> indent 0 "On the " ++ o1 ++ indent 2 (pPrint u1) ++ "\n"  
    Anytime (O(o1,o2)) u -> indent 0 "Contract executable between " ++ o1 ++ indent 2 (pPrint u)
    Until (O(o1,o2)) u -> indent 0 "Until " ++ o1 ++ " " ++ pPrint u ++ "\n"

-- OLD CODE ---------------------------------------------------------------------------------------------------------------------------------------------------------------
--Date Manipulation------
--Returns the day the contract expires
horizon :: Date -> Day
horizon (C(t1,t2,t3)) = getDay (C(t1,t2,t3))

--returns the further/nearer date from now ("day 0") in days
maxDate :: Date -> Date -> Date
maxDate (C(t1,t2,t3))(C(s1,s2,s3)) = max (C(t1,t2,t3)) (C(s1,s2,s3))

minDate :: Date -> Date -> Date
minDate (C(t1,t2,t3))(C(s1,s2,s3)) = min (C(t1,t2,t3)) (C(s1,s2,s3))