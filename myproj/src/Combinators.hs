{-


-}

--Notational conventions from paper
--c, d, u : Contract
--      o : Observable
--   t, s : Date, time
--      k : Currency
--      x : Dimensionless real value
--      p : Value process
--      v : Random variable

module Combinators where

import StringToDate 
import Observable


data Currency = USD | GBP | EUR | RMB | JPY | CHF  deriving (Eq, Show, Read)

--Representation of a contract
data Contract =
    Zero
  | One  Currency
  | Give Contract
  | And  Contract Contract
  | Or   Contract Contract
  | Cond    (Obs Bool) Contract Contract   --Obs Bool
  | Scale   (Obs Double) Contract            --Obs a where Num a or Obs Double
  | When    (Obs Bool) Contract            --Obs Bool
  | Anytime (Obs Bool) Contract            --Obs Bool
  | Until   (Obs Bool) Contract            --Obs Bool
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


konst :: Obs a -> b -> Obs b
konst (O(o1,o2,_)) x = (O(o1,o2,x))

at :: Date -> Obs Bool
at t 
           |mkDate  == t = O ("?",(C(0,0,0)),True) 
           |otherwise = O ("?",(C(0,0,0)),False)

--Forward (fwd) Contract Defintion
--fwd :: Obs a -> Currency -> Contract
fwd (O(m,t,x)) k =  cWhen (at t) (scale (konst (O(m,t,x))(valObs(O(m,t,x)))) (one k)) 

swap ::  Contract -> Contract -> Contract
swap c1 c2 =   c1 `cAnd` (give c2)

european :: Date -> Contract -> Contract
european t c = cWhen (at t) (c `cOr` zero)

american :: (Date, Date) -> Contract -> Contract
american (t1, t2) c = anytime (between t1 t2) c

between :: Date -> Date -> Obs Bool
between t1 t2 = konst (O("?", mkDate, 0))(mkDate >= t1 && mkDate <= t2)

--Printing
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
--Naive Print, represent all the things!!
represent :: Contract -> String
represent c = case c of
    Zero -> "Contract no obligations, no rights."
    One k->  show k
    Give u -> "PAY " ++ represent u
    And u1 u2-> represent u1 ++ " AND " ++ represent u2
    Or u1 u2 -> "OPTION \n" ++ represent u1 ++ "OR " ++ "OPTION \n" ++ represent u2
    Cond (O(_,_,o3)) u1 u2 -> "Carry on"
    Scale (O(o1,o2,o3)) u -> show o1 ++ " nominal " ++ represent u ++ " " ++ show o3 ++ " on the " ++ date2String o2 ++ "\n"
    When (O(_,_,o3)) u1-> "Contract \n" ++ represent u1
    Anytime (O(_,_,o3)) u -> "Savage"
    Until (O(_,o2,o3)) u -> "this may work"
    _ -> "something is not right"  




--mcmahon/git examples of contracts (fwd adapted from zcb)

-- --Other combinator derived from the primitives above
-- andGive :: Contract -> Contract -> Contract
-- andGive c d = c `cAnd` give d

-- --Single swap, (interest rates) exchange, could be seen as a combination of a long fwd and short fwd


-- c1 :: Contract
-- c1 = zcb t1 10 USD

-- t1 :: Date
-- t1 = mkDate t1Horizon

-- t1Horizon :: TimeStep
-- t1Horizon = 3

c11 :: Contract
c11 = european (mkDate)
        (fwd (O("Bond E Interest",(C(2019,04,10)), 0.4)) USD `cAnd`
        fwd (O("Bond E Interest",(C(2019,04,15)), 9.3)) USD `cAnd`
        fwd (O("Bond E Interest and Principal",(C(2019,05,10)),109.3)) USD `cAnd`
        give ((fwd (O("Option Premium",(C(2019,04,10)), 100)) USD)))
