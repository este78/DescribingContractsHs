module Observable where


import StringToDate

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







-- newtype Date = Day Int deriving (Show, Eq, Ord)

-- --Create your own date
-- mkDate :: Int -> Date
-- mkDate s = Day s

-- time0 :: Date
-- time0 = mkDate 0

-- --Returns the day the contract expires
-- horizon :: Date -> Int
-- horizon (Day a) = a

-- --Returns the difference between two dates in Int
-- dayDiff :: Date -> Date -> Int
-- dayDiff (Day a) (Day b) = b - a

-- --returns the further/nearer date from now ("day 0") in days
-- maxDate :: Date -> Date -> Date
-- maxDate (Day a)(Day b) = Day $ max a b

-- minDate :: Date -> Date -> Date
-- minDate (Day a)(Day b) = Day $ min a b

-- --returns amount of months from Day 0 to time Day x
-- dayToMonth :: Date -> Int
-- dayToMonth (Day a) =
        -- if a `mod` 30 == 0
          -- then a `div` 30
          -- else (a `div` 30) + 1


-- data Obs a = O (String, Date, Double) |O' Bool | O'' Double deriving (Show, Read, Eq, Ord)

-- --Observation to string
-- kindOfObs :: Show a => Obs a -> String
-- kindOfObs (O (a, b, c)) = a ++ " " ++ (show b) ++ " at " ++ (show c)

-- --return name of observation
-- nameObs :: Obs a-> String
-- nameObs (O (a, _, _)) = a

-- --return date associated to the observation
-- dateObs :: Obs a -> Date
-- dateObs (O (_, a, _)) = a

-- --real-numerical value associated to O
-- valObs :: Obs a -> Double
-- valObs (O (_, _, a)) = a

-- --pass observation
-- createObs :: String -> Date -> Double -> Obs a
-- createObs a b c = O (a, b, c)


-- --Code from rossng/merchant github  
-- --{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, GADTs #-}
-- -- data Obs a where
  -- -- External :: String -> Obs a
  -- -- Constant :: a -> Obs a
  -- -- After :: Date -> Obs Bool
  -- -- Before :: Date -> Obs Bool
  -- -- At :: Date -> Obs Bool
  -- -- OAnd :: Obs Bool -> Obs Bool -> Obs Bool
  -- -- OGreaterThan :: Obs Int -> Obs Int -> Obs Bool
  -- -- OSubtract :: Obs Int -> Obs Int -> Obs Int
  

-- -- between :: Date -> Date -> Obs Bool
-- -- between t1 t2 = OAnd (After t1) (Before t2)


