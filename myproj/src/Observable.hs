module Observable where



type DayInYear = 1 | 360 

data Obs a where 
	Constant :: a -> Obs a
	At :: Time -> Obs Bool
	dayDiff :: Obs Int -> Obs Int -> Obs Int

	
konst a :: (Show a, Eq a) => a -> Obs a
konst a = Constant a


--Getting today's date?
-- import Data.Time.Clock
-- import Data.Time.Calendar

-- date :: IO (Integer,Int,Int) -- :: (year,month,day)
-- date = getCurrentTime >>= return . toGregorian . utctDay