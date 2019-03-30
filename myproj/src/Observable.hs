-- From rossng/merchant   
{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, GADTs #-}
module Observable where



data Date = Day Int deriving (Show, Eq, Ord)

horizon :: Date -> Int
horizon (Day a) = a

dayDiff :: Date -> Date -> Int
dayDiff (Day a) (Day b) = b - a

maxDate :: Date -> Date -> Date
maxDate (Day a)(Day b) = Day (max a b)





data Obs a where
  External :: String -> Obs a
  Constant :: a -> Obs a
  After :: Date -> Obs Bool
  Before :: Date -> Obs Bool
  At :: Date -> Obs Bool
  OAnd :: Obs Bool -> Obs Bool -> Obs Bool
  OGreaterThan :: Obs Int -> Obs Int -> Obs Bool
  OSubtract :: Obs Int -> Obs Int -> Obs Int
  

between :: Date -> Date -> Obs Bool
between t1 t2 = OAnd (After t1) (Before t2)


