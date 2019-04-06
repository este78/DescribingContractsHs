{-


-}


module Combinators where

import StringToDate 

--newtype Date = Day Int deriving (Show, Read, Eq, Ord)

--Create your own date
mkDate ::  Date
mkDate = (C(2019,4,6))

time0 :: Date
time0 = C(0,0,0)

--Returns the day the contract expires
horizon :: Date -> Int
horizon (C(a,b,c)) =  getDay (C(a,b,c))

--Returns the difference between two dates in Int
dayDiff :: Date -> Date -> Int
dayDiff (C(a,b,c)) (C(x,y,z)) = getDay(C(a,b,c)) - getDay(C(x,y,z))

--returns the further/nearer date from now ("day 0") in days
maxDate :: Date -> Date -> Date
maxDate (C(a,b,c))(C(x,y,z)) = max (C(a,b,c)) (C(x,y,z))

minDate :: Date -> Date -> Date
minDate (C(a,b,c))(C(x,y,z)) = min (C(a,b,c)) (C(x,y,z))

--Operations adding/subtracting days to a Date
incrementDate :: Date -> Int -> Date
incrementDate (C(a,b,c)) x = toDate(getDay(C(a,b,c)) + x)


data Obs a = O (String, Date, a)
  deriving (Show, Read)

--Observation to string
kindOfObs :: Show a => Obs a -> String
kindOfObs (O (a, b, c)) = a ++ " " ++ (show b) ++ " at " ++ (show c)

--return name of observation
nameObs :: Obs a-> String
nameObs (O (a, _, _)) = a

--return date associated to the observation
dateObs :: Obs a -> Date
dateObs (O (_, a, _)) = a

--real-numerical value associated to O
valObs :: Obs a -> a
valObs (O(_, _, a)) = a

--pass observation
createObs :: String -> Date -> Double -> Obs Double
createObs a b c = O (a, b, c)



--Notational conventions from paper
--c, d, u : Contract
--      o : Observable
--   t, s : Date, time
--      k : Currency
--      x : Dimensionless real value
--      p : Value process
--      v : Random variable

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
konst (O(s,t,_)) x = (O(s,t,x))

at :: Date -> Obs Bool
at t 
           |mkDate  == t = O ("?",(C(0,0,0)),True) 
           |otherwise = O ("?",(C(0,0,0)),False)

--Forward Contract Defintion
--forward :: Obs a -> Currency -> Contract
forward (O(s,t,x)) k =  cWhen (at t) (scale (konst (O(s,t,x))(valObs(O(s,t,x)))) (one k)) 
 
swap ::  Contract -> Contract -> Contract
swap c1 c2 =   c1 `cAnd` (give c2)

european :: Date -> Contract -> Contract
european t u = cWhen (at t) (u `cOr` zero)

american :: (Date, Date) -> Contract -> Contract
american (t1, t2) u = anytime (between t1 t2) u

between :: Date -> Date -> Obs Bool
between t1 t2 = konst (O("?", mkDate, 0))(mkDate >= t1 && mkDate <= t2)


 
--Printing 
printForward :: (Show a1, Show a2)=> Obs a1 -> a2 -> String
printForward (O(s,t,x)) k = let b = (at t) 
    in case b of 
        (O(_, _, True))
          -> "Exercised " ++ s ++ " " ++ (show x) ++ " " ++ (show k) 
        (O(_, _, False)) 
          -> "Expecting to exercise " ++ s ++ " " ++ (show x) ++ " " ++ (show k) ++ " in " ++(show t)
 

printContract c = show c
                 

----------------------------------------------------------


--Printing
--case (cWhen (at t)) of (O' True) -> ("Exercised" ++ " " ++ (show x) ++ " " ++ (show k))
                      -- (O' False) ->  ("Expecting on" ++ (show t) ++ " " ++ printKonst(konst (O(s,t,x))(valObs(O(s,t,x)))) ++ " " ++ (show k))

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

-- c11 :: Contract
-- c11 = european (mkDate 2)
        -- (forward (mkDate 20) 0.4 USD `cAnd`
        -- forward (mkDate 30) 9.3 USD `cAnd`
        -- forward (mkDate 40) 109.3 USD `cAnd`
        -- give (forward (mkDate 12) 100 USD))
