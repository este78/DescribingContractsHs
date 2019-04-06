{-


-}


module CombinatorsB where

import StringToDate
import System.IO

--Create your own date
-- mkDate :: Int -> Date 
-- mkDate s = Day s



data Obs a = O (String, Date, a)
  deriving (Show, Read, Eq, Ord)

--Observation to string
printObsA :: Show a => Obs a -> String
printObsA (O (a, b, c)) = a ++ " " ++ (show b) ++ " at " ++ (show c)

--return name of observation
valName :: Obs a-> String
valName (O (a, _, _)) = a

--return date associated to the observation
valDate :: Obs a -> Date 
valDate (O (_, a, _)) = a

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


     

konst :: Obs a -> Double -> Obs Double
konst (O(s,t,_)) x = (O(s,t,x))

--at :: Date -> IO Date -> Obs Bool
at t = do 
     a <- (dateIO t)
     return (O ("?",t,a)) 
         

--Forward Contract Defintion
forward :: Obs a -> Currency -> Contract
forward (O(s,t,x)) k =  cWhen (at t) (scale (konst (O(s,t,x))(valObs(O(s,t,x)))) (one k)) 
 
--Printing 
--printForward :: (Show a1, Show a2)=> Obs a1 -> a2 -> String
printForward (O(s,t,x)) k = do
      b <- (at t) 
      return  ("Contract Exercised?: " ++(show (valObs b) ++ " " ++ show (valDate b)) ++ ", " ++ s ++ " " ++ (show x) ++ " " ++ (show k)) 
        
                                                            

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
