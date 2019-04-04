{-

Suggestions below - just load into ghci and fix error messages

Next meeting: 12noon Thu 4th April
-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Combinators where

newtype Date = Day Int deriving (Show, Read, Eq, Ord)

--Create your own date
mkDate :: Int -> Date
mkDate s = Day s

time0 :: Date
time0 = mkDate 0

--Returns the day the contract expires
horizon :: Date -> Int
horizon (Day a) = a

--Returns the difference between two dates in Int
dayDiff :: Date -> Date -> Int
dayDiff (Day a) (Day b) = b - a

--returns the further/nearer date from now ("day 0") in days
maxDate :: Date -> Date -> Date
maxDate (Day a)(Day b) = Day $ max a b

minDate :: Date -> Date -> Date
minDate (Day a)(Day b) = Day $ min a b

--returns amount of months from Day 0 to time Day x
dayToMonth :: Date -> Int
dayToMonth (Day a) =
        if a `mod` 30 == 0
          then a `div` 30
          else (a `div` 30) + 1


data Obs a = O (String, Date, Double) |O' Bool deriving (Show, Read, Eq, Ord)

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
valObs :: Obs a -> Double
valObs (O(_, _, a)) = a

--pass observation
createObs :: String -> Date -> Double -> Obs a
createObs a b c = O (a, b, c)



--Notational conventions from paper
--c, d, u : Contract
--      o : Observable
--   t, s : Date, time
--      k : Currency
--      x : Dimensionless real value
--      p : Value process
--      v : Random variable

data Currency = USD | GBP | EUR | KYD | ZAR | CHF  deriving (Eq, Show, Read)

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


     

konst :: Obs a -> Double -> Obs Double
konst (O(s,t,_)) x = (O(s,t,x))

at :: Date -> Obs Bool
at t 
           |(mkDate 100) == t = O' True 
           |otherwise = O' False

--Forward Contract Defintion
forward :: Obs a -> Currency -> Contract
forward (O(s,t,x)) k =  cWhen (at t) (scale (konst (O(s,t,x))(valObs(O(s,t,x)))) (one k)) 
 
--Printing 
printForward :: (a->b->c) -> Obs a -> Currency-> String
printForward forward (O(s,t,x)) k = let b = (at t) in case b of (O' True) -> "Exercised " ++ s ++ " " ++ (show x) ++ " " ++ (show k) 
                                                                (O' False) -> "Expecting to exercise " ++ s ++ " " ++ (show x) ++ " " ++ (show k) ++ " in " ++(show t)
                                                            

--Printing
--case (cWhen (at t)) of (O' True) -> ("Exercised" ++ " " ++ (show x) ++ " " ++ (show k))
                      -- (O' False) ->  ("Expecting on" ++ (show t) ++ " " ++ printKonst(konst (O(s,t,x))(valObs(O(s,t,x)))) ++ " " ++ (show k))

--mcmahon/git examples of contracts (forward adapted from zcb)

-- --Other combinator derived from the primitives above
-- andGive :: Contract -> Contract -> Contract
-- andGive c d = c `cAnd` give d

-- --Single swap, (interest rates) exchange, could be seen as a combination of a long forward and short forward

-- -- swap :: Date -> Contract -> Contract -> Contract
-- -- swap t c1 c2 = when (at t) forward t x k 'and' give(forward t x k)  --Should we use a class for forwards?

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
