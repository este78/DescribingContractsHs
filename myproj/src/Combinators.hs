module Combinators where

data Date = Day Int deriving (Show, Eq, Ord)

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

--returns number of months
dayToMonth :: Date -> Int
dayToMonth (Day a) = 
        if a `mod` 30 == 0
          then a `div` 30 
          else (a `div` 30) + 1


data Obs = O (String, Date, Double) deriving (Show, Eq, Ord)

--Observation to string
kindOfObs :: Obs -> String 
kindOfObs (O (a, b, c)) = a ++ " " ++ (show b) ++ " at " ++ (show c)

--return name of observation
nameObs :: Obs -> String
nameObs (O (a, _, _)) = a

--return date associated to the observation
dateObs :: Obs -> Date
dateObs (O (_, a, _)) = a

--real-numerical value associated to Obs 
valObs :: Obs -> Double
valObs (O (_, _, a)) = a

--pass observation
createObs :: String -> Date -> Double -> Obs
createObs a b c = O (a, b, c)

--Check dates
checkDate :: Obs -> Date -> Bool
checkDate (O(_,a,_)) b = if (a == b) then True else False

--Notational conventions from paper
--c, d, u : Contract
--      o : Observable
--   t, s : Date, time
--      k : Currency
--      x : Dimensionless real value
--      p : Value process
--      v : Random variable

data Currency = USD | GBP | EUR | KYD | ZAR | CHF  deriving (Eq, Show)

--Representation of a contract
data Contract =
    Zero
  | One  Currency
  | Give Contract
  | And  Contract Contract
  | Or   Contract Contract
  | Cond    Obs Contract Contract   --Obs Bool
  | Scale   Obs Contract
  | When    Obs Contract            --Obs Bool
  | Anytime Obs Contract            --Obs Bool
  | Until   Obs Contract            --Obs Bool
  deriving Show


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

scale :: Obs -> Contract -> Contract
scale = Scale

cond :: Obs -> Contract -> Contract -> Contract
cond = Cond

cWhen :: Obs -> Contract -> Contract
cWhen = When

anytime :: Obs -> Contract -> Contract
anytime = Anytime

cUntil :: Obs -> Contract -> Contract
cUntil = Until

--Other combinator derived from the primitives above
andGive :: Contract -> Contract -> Contract
andGive c d = c `cAnd` give d