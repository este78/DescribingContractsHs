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

--Forward Contract Defintion
--forward :: Obs a -> Currency -> Contract
forward (O(m,t,x)) k =  cWhen (at t) (scale (konst (O(m,t,x))(valObs(O(m,t,x)))) (one k)) 

swap ::  Contract -> Contract -> Contract
swap c1 c2 =   c1 `cAnd` (give c2)

european :: Date -> Contract -> Contract
european t c = cWhen (at t) (c `cOr` zero)

american :: (Date, Date) -> Contract -> Contract
american (t1, t2) c = anytime (between t1 t2) c

between :: Date -> Date -> Obs Bool
between t1 t2 = konst (O("?", mkDate, 0))(mkDate >= t1 && mkDate <= t2)


 
--Printing 
--printForward :: (Show a1, Show a2)=> Obs a1 -> a2 -> String
printContract2 (O(s,t,x)) k = let b = (at t) 
         in case b of 
             (O(_, _, True))
               -> "Exercised " ++ s ++ " " ++ (show x) ++ " " ++ (show k) 
             (O(_, _, False)) 
               -> "Expecting to exercise " ++ s ++ " " ++ (show x) ++ " " ++ (show k) ++ " in " ++(show t)
 

represent :: Contract -> String

represent c = case c of
    Zero -> "No Obligations, no Rights."
    One k->  show k
    Give u -> "Pay " ++ represent u
    And u1 u2-> represent u1 ++ " AND " ++ represent u2
    Or u1 u2 -> "OPTION \n" ++ represent u1 ++ "\nOR " ++ "OPTION \n" ++ represent u2
    Cond (O(_,_,o3)) u1 u2 -> "Carry on"
    Scale (O(o1,o2,o3)) u -> show o1 ++ " nominal " ++ represent u ++ " " ++ show o3 ++ " on the " ++ date2String o2 ++ "\n"
    When (O(_,_,o3)) u1-> "Receive " ++ represent u1
    Anytime (O(_,_,o3)) u -> "Savage"
    Until (O(_,o2,o3)) u -> "this may work"
    _ -> "something is not right"  
     
               

----------------------------------------------------------


--Printing

--mcmahon/git examples of contracts (forward adapted from zcb)

-- --Other combinator derived from the primitives above
-- andGive :: Contract -> Contract -> Contract
-- andGive c d = c `cAnd` give d

-- --Single swap, (interest rates) exchange, could be seen as a combination of a long forward and short forward


-- c1 :: Contract
-- c1 = zcb t1 10 USD

-- t1 :: Date
-- t1 = mkDate t1Horizon

-- t1Horizon :: TimeStep
-- t1Horizon = 3

c11 :: Contract
c11 = european (mkDate)
        (forward (O("Bond E Interest",(C(2019,04,10)), 0.4)) USD `cAnd`
        forward (O("Bond E Interest",(C(2019,04,15)), 9.3)) USD `cAnd`
        forward (O("Bond E Interest and Principal",(C(2019,05,10)),109.3)) USD `cAnd`
        give ((forward (O("Option Premium",(C(2019,04,10)), 100)) USD)))
