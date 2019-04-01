module Observable where


newtype Date = Day Int deriving (Show, Eq, Ord)

--Create your own date
mkDate :: Int -> Date
mkDate s = Day s

time0 :: Date
time0 = mkDate 0

--Returns the day the contract expires
horizon :: Date -> Int
horizon (Day a) = a

--Returns the difference between two dates in Int
dayDiff :: Date -> Date -> Int
dayDiff (Day a) (Day b) = b - a

--returns the further/nearer date from now ("day 0") in days
maxDate :: Date -> Date -> Date
maxDate (Day a)(Day b) = Day $ max a b

minDate :: Date -> Date -> Date
minDate (Day a)(Day b) = Day $ min a b

--returns amount of months from Day 0 to time Day x
dayToMonth :: Date -> Int
dayToMonth (Day a) = 
        if a `mod` 30 == 0
          then a `div` 30 
          else (a `div` 30) + 1
 

data Observable = Obs (String, Date, Double)  deriving (Show, Eq, Ord)

--Observation to string
kindOfObs :: Observable -> String 
kindOfObs (Obs (a, b, c)) = a ++ " " ++ (show b) ++ " at " ++ (show c)

--return name of observation
nameObs :: Observable -> String
nameObs (Obs (a, _, _)) = a

--return date associated to the observation
dateObs :: Observable -> Date
dateObs (Obs (_, a, _)) = a

--real-numerical value associated to Obs 
valObs :: Observable -> Double
valObs (Obs (_, _, a)) = a

--pass observation
createObs :: String -> Date -> Double -> Observable
createObs a b c = Obs (a, b, c)

--Code from rossng/merchant github  
--{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, GADTs #-}
-- data Obs a where
  -- External :: String -> Obs a
  -- Constant :: a -> Obs a
  -- After :: Date -> Obs Bool
  -- Before :: Date -> Obs Bool
  -- At :: Date -> Obs Bool
  -- OAnd :: Obs Bool -> Obs Bool -> Obs Bool
  -- OGreaterThan :: Obs Int -> Obs Int -> Obs Bool
  -- OSubtract :: Obs Int -> Obs Int -> Obs Int
  

-- between :: Date -> Date -> Obs Bool
-- between t1 t2 = OAnd (After t1) (Before t2)


