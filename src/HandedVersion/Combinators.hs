module Combinators where

import NewObs
import CombDate

-- =====================================================================================================================================================================
-- COMBINATORS
-- =====================================================================================================================================================================
--
data Condition = 
      At (Obs Day)
    | IsTrue (Obs Bool)
    | Comp (Obs Double)(Obs Double)
    | Between (Obs Date) (Obs Date)
    deriving (Show, Read)
--

data Currency = USD | GBP | EUR | RMB | JPY | CHF 
                | Rate String 
                deriving (Eq, Show, Read)

--Representation of a contract
data Contract =
    Zero
  | One  Currency
  | Give Contract
  | And  Contract Contract
  | Or   Contract Contract
  | Cond    Condition Contract Contract   
  | Scale   (Obs Double) Contract          
  | When    Condition Contract
  | Anytime Condition Contract            
  | Until   Condition Contract 
  | Empty   
  deriving (Show, Read)


--Primitives for Defining Contracts
zero :: Contract
zero = Zero                                 

one :: Currency -> Contract                  
one = One

give :: Contract -> Contract                 
give = Give

cAnd :: Contract -> Contract -> Contract     
cAnd = And

cOr :: Contract -> Contract -> Contract
cOr = Or

scale :: (Obs Double)  -> Contract -> Contract
scale = Scale

cond :: Condition -> Contract -> Contract -> Contract
cond = Cond

cWhen :: Condition -> Contract -> Contract
cWhen = When

anytime :: Condition -> Contract -> Contract
anytime = Anytime

cUntil :: Condition -> Contract -> Contract
cUntil = Until

empty :: Contract
empty = Empty

-- ==========================================================================
--                  OTHER USEFUL FUNCTIONS - ACTUALLY NOT IN USE       
-- ==========================================================================
--Create your own date

time0 = (C(2020,1,1))
mkDate d = incrementDate d (Day 1)

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

-- ==========================================================================
--  Examples of Types of Contracts
-- ==========================================================================
--Forward (fwd) Contract Defintion
--fwd :: Obs a -> Currency -> Contract
fwd t q k =  cWhen (t) (scale ((konst q)(valObs q)) (one k)) 

swap ::  Contract -> Contract -> Contract
swap c1 c2 =   c1 `cAnd` (give c2)

--european :: Obs Date -> Contract -> Contract
european t c = cWhen (t) (c `cOr` zero)

--american :: Date -> Date -> Contract -> Contract
american t1 t2 c = anytime (IsTrue $ between t1 t2) (c `cOr` zero)
