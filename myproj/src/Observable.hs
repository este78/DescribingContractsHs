-- From rossng/merchant   
{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, GADTs #-}
module Observable where



data Date = Day Int deriving (Show, Eq, Ord)

--Returns the day 
horizon :: Date -> Int
horizon (Day a) = a

--Returns the difference between two dates in Int
dayDiff :: Date -> Date -> Int
dayDiff (Day a) (Day b) = b - a

--returns the higher date in days
maxDate :: Date -> Date -> Date
maxDate (Day a)(Day b) = Day $ max a b

--returns number of months
dayToMonth :: Date -> Int
dayToMonth (Day a) = 
        if a `mod` 30 == 0
          then a `div` 30 
          else (a `div` 30) + 1



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


