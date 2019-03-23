import Data.Time.Clock
import Data.Time.Calendar

-- Describe a simple forward contract.
type Contract = String 
-- Holder will receive €100 at a future date t.
-- We need to address currency, quantity of currency, date of payment.
-- Forward is similar to the zcb example

one :: Currency -> Contract          			 	   	--base quantity

konst :: a -> Obs a								 
scale :: Obs Double -> Contract -> Contract
scaleK :: Double -> Contract -> Contract
scaleK x c = scale (konst x) c                   	   	--This 4 lines scale "one" to the specified observable amount


--how to determine what currency k is being used? (List of Currencies?)

--date :: IO (Integer,Int,Int) -- :: (year,month,day)    How does Obs Date work? it must be an impure value. 
--date = getCurrentTime >>= return . toGregorian . utctDay   

date :: Obs Date                                 	   	--returns current date
												 
at :: Date -> Obs Bool                           	   	--Boolean, is the current date the same as the specified (t)
at t = lift2 (==) date (konst t)                 	 	--lift2 apply equality check to

when :: Obs Bool -> Contract -> Contract				--Acquiring c immediately at the next instance when o becomes true.

forward :: Date -> Double -> Currency -> Contract 		--Long forward when t is true (==current date), receive (one k) * x
forward t x k = when (at t) (scaleK x (one k))
--Short position
forward t x k = give(when (at t) (scaleK x (one k)))	--give swaps rights/obligations in the contract. 
														--In this case the seller pays (one k) *x
														
--==================================================================================================================================================														

--Single swap, (interest rates) exchange, could be seen as a combination of a long forward and short forward
 
swap :: Date -> Contract -> Contract -> Contract
swap t c1 c2 = when (at t) forward t x k 'and' give(forward t x k)  --Should we use a class for forwards?
