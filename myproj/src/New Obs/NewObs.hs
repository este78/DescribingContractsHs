{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NewObs where


-- ====================================================================================================================================================================
--  OBSERVABLES
-- ====================================================================================================================================================================

data Obs a = O (String, Date, a)
  deriving (Show, Read)

--Observation to string
kindOfObs :: Show a => Obs a -> String
kindOfObs (O (o1, o2, o3)) = o1 ++ " " ++ (show o2) ++ " at " ++ (show o3)

--return name of observation
nameObs :: Obs a-> String
nameObs (O (o1, _, _)) = o1

--return date associated to the observation
dateObs :: Obs a -> Date
dateObs (O (_, o2, _)) = o2

--real-numerical value associated to O
valObs :: Obs a -> a
valObs (O(_, _, o3)) = o3

--pass observation
createObs :: String -> Date -> Double -> Obs Double
createObs o1 o2 o3 = O (o1, o2, o3)
--




-- ===================================================================================================================================================================
--  DATE  
-- 4. add Days datatype
-- 5. Implement add :: Date -> Days -> Date
-- 6. Implement diff :: Date -> Date -> Days
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

mkDate ::  Date
mkDate = (C(2019,4,6))

time0 :: Date
time0 = C(0,0,0)

--Returns the day the contract expires
horizon :: Date -> Day
horizon (C(t1,t2,t3)) = getDay (C(t1,t2,t3))

--Returns the difference between two dates in Int
--dateDiff :: Date -> Date -> Day
dateDiff (C(t1,t2,t3)) (C(s1,s2,s3)) = getDay(C(t1,t2,t3)) - getDay(C(s1,s2,s3))

--returns the further/nearer date from now ("day 0") in days
maxDate :: Date -> Date -> Date
maxDate (C(t1,t2,t3))(C(s1,s2,s3)) = max (C(t1,t2,t3)) (C(s1,s2,s3))


minDate :: Date -> Date -> Date
minDate (C(t1,t2,t3))(C(s1,s2,s3)) = min (C(t1,t2,t3)) (C(s1,s2,s3))


--Operations adding/subtracting days to a Date
--incrementDate :: Date -> Day -> Date
incrementDate (C(t1,t2,t3)) x = toDate(getDay(C(t1,t2,t3)) + x)

date2String :: Date -> String
date2String (C(t1,t2,t3))= show t1 ++ "/" ++ show t2 ++ "/" ++ show t3  

--Converts a Calendar date in the form year,month,day into a integer
--------------------------------------------------------------------------------------------------------
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
                        in C(dYear y m, dMonth m, dDay d m)
                      

--Day to Date Inner Workings
--Intermediate Step to calculate the date
--convertToYear :: Integer -> Integer
convertToYear g = (10000*g + 14780) `div` 3652425

--convertToDays :: Int -> Integer -> Int
convertToDays g y = g - (365*y + y `div` 4 - y `div` 100 + y `div` 400)

--convertToMonth :: Int -> Int
convertToMonth d = (100*d + 52) `div` 3060

--Final Step to Calculate de Date
--dYear :: Integer -> Int -> Integer
dYear y m = let m' = toInteger ((m + 2) `div` 12) in y + m'

dDay d m = fromInteger (d - ((m*306 + 5) `div` 10) + 1)

--dMonth :: Int -> Int
dMonth m = fromInteger(((m + 2) `mod` 12) + 1)


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

anytime :: Obs Bool-> Contract -> Contract
anytime = Anytime

cUntil :: Obs Bool-> Contract -> Contract
cUntil = Until




-- ======================================================
-- TODO
-- ======================================================
konst :: Obs a -> b -> Obs b
konst (O(o1,o2,_)) x = (O(o1,o2,x))

at :: Date -> Obs Bool
at t 
           |mkDate  >= t = O("?",t ,True) 
           |otherwise = O("?",t, False)

--Forward (fwd) Contract Defintion
--fwd :: Obs a -> Currency -> Contract
fwd (O(m,t,x)) k =  cWhen (at t) (scale (konst (O(m,t,x))(valObs(O(m,t,x)))) (one k)) 

swap ::  Contract -> Contract -> Contract
swap c1 c2 =   c1 `cAnd` (give c2)

european :: Obs a -> Contract -> Contract
european (O(m,t,x)) c = cWhen (at t) (c `cOr` zero)

--american :: (Date, Date) -> Contract -> Contract
american (t1,t2) c = anytime (between t1 t2) (c `cOr` zero)

between :: Date -> Date -> Obs Bool
between t1 t2 = konst (O("?", mkDate, 0))(mkDate >= t1 && mkDate <= t2)

--Printing
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
--Naive Print, represent all the things!!
represent :: Contract -> String
represent c = case c of
    Zero -> "Contract no obligations, no rights.\n"
    One k->  show k
    Give u -> "PAY " ++ represent u
    And u1 u2-> represent u1 ++ " AND \n" ++ represent u2
    Or u1 u2 -> "\nOPTION \n" ++ represent u1 ++ "\nOR " ++ "OPTION \n" ++ represent u2
    Cond (O(_,o2,o3)) u1 u2 -> "Carry on"
    Scale (O(o1,o2,o3)) u -> show o1  ++ represent u ++ " " ++ show o3 ++ "\n"
    When (O(o1,o2,o3)) u1-> "Contract " ++ o1 ++ " " ++ date2String o2 ++ "\n" ++ represent u1
    Anytime (O(_,t2,_)) u -> "Can be exercised between " ++ " and " ++ date2String t2 ++ " "++ represent u
    Until (O(_,o2,_)) u -> "Valid until " ++ date2String o2
    _ -> "something is not right"  


