module NewObs where

import CombDate

-- ====================================================================================================================================================================
--  OBSERVABLES
-- ====================================================================================================================================================================

-- An Observable is a time dependent variable.
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

--Two Obs Eq comparison
eqComp o1 o2 = (nameObs o1 ==  nameObs o2 && valObs o1 == valObs o2)











--OLD CODE ---------------------------------------------------------------------------------------------------------------------------------------------------------------
--Date Manipulation------
--Returns the day the contract expires
-- horizon :: Date -> Day
-- horizon (C(t1,t2,t3)) = getDay (C(t1,t2,t3))

-- --returns the further/nearer date from now ("day 0") in days
-- maxDate :: Date -> Date -> Date
-- maxDate (C(t1,t2,t3))(C(s1,s2,s3)) = max (C(t1,t2,t3)) (C(s1,s2,s3))

-- minDate :: Date -> Date -> Date
-- minDate (C(t1,t2,t3))(C(s1,s2,s3)) = min (C(t1,t2,t3)) (C(s1,s2,s3))

-- Checking Equivalence of Obs For Conditonal
--comp (Comp o1 o2) = O((nameObs o1 ++ " is " ++ show (valObs o1)), valObs o1 == valObs o2)