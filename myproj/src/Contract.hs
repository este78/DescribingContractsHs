--This program was developed by, and is copyright © 2007 by Anton van Straaten. 
--It may be freely used and copied for educational purposes.

module Contract where
 
--import List
import Numeric
import Control.Monad
import Data.Maybe
--import System
--import Text.XHtml.Strict
import Data.Unique

--Notational conventions from paper
--c, d, u : Contract
--      o : Observable
--   t, s : Date, time
--      k : Currency
--      x : Dimensionless real value
--      p : Value process
--      v : Random variable

data Currency = USD | GBP | EUR | KYD | ZAR | CHF  deriving (Eq, Show)

type Date = (CalendarTime, TimeStep)
type TimeStep = Int
type CalendarTime = ()

mkDate :: TimeStep -> Date
mkDate s = ((),s)

time0 :: Date
time0 = mkDate 0

--Representation of a contract
data Contract =
    Zero
  | One  Currency
  | Give Contract
  | And  Contract Contract
  | Or   Contract Contract
  | Cond    (Obs Bool)   Contract Contract
  | Scale   (Obs Double) Contract
  | When    (Obs Bool)   Contract
  | Anytime (Obs Bool)   Contract
  | Until   (Obs Bool)   Contract
  deriving Show

-- --newtype Obs a = Obs (Date -> a)
-- newtype Obs a = Obs (Date -> a)

-- instance Show a => Show (Obs a) where
  -- show (Obs o) = "(Obs " ++ show o ++ ")"


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

cond :: Obs Bool -> Contract -> Contract -> Contract
cond = Cond

scale :: Obs Double -> Contract -> Contract
scale = Scale

cWhen :: Obs Bool -> Contract -> Contract
cWhen = When

anytime :: Obs Bool -> Contract -> Contract
anytime = Anytime

cUntil :: Obs Bool -> Contract -> Contract
cUntil = Until

--Other combinator derived from the primitives above
andGive :: Contract -> Contract -> Contract
andGive c d = c `cAnd` give d

-- --Primitives over observables 
-- konst :: a -> Obs a					--konst x is an observable that has value x at any time
-- konst k = Obs (\t -> bigK k)


-- lift :: (a -> b) -> Obs a -> Obs b
-- lift f (Obs o) = Obs (map f o)

-- lift2 :: (a -> b -> c) -> Obs a -> Obs b -> Obs c
-- lift2 f (Obs o1) (Obs o2) = Obs (f (f o1) (f o2))

-- --"The value of the observable date at date t is just t."
-- date :: Obs Date
-- date = Obs (\t -> t)

-- --"All numeric operations lift to the Obs type. The implementation is simple,
-- --using lift and lift2."
-- instance Num a => Num (Obs a) where
  -- fromInteger i = konst (fromInteger i)
  -- (+) = lift2 (+)
  -- (-) = lift2 (-)
  -- (*) = lift2 (*)
  -- abs = lift abs
  -- signum = lift signum

-- --We need to define a stub for Eq to support the Num instance.
-- instance Eq a => Eq (Obs a) where
  -- (==) = undefined

-- --We can't implement Eq on an Observable's function,
-- --but we can provide a lifted version of equality:
-- (==*) :: Ord a => Obs a -> Obs a -> Obs Bool
-- (==*) = lift2 (==)

-- at :: Date -> Obs Bool       --boolean observable that becomes True at time t
-- at t = date ==* konst t

-- --defining relational operations to work as typeclasses
-- (%<), (%<=), (%=), (%>=), (%>) :: Ord a => Obs a -> Obs a -> Obs Bool
-- (%<)  = lift2 (<)
-- (%<=) = lift2 (<=)
-- (%=)  = lift2 (==)
-- (%>=) = lift2 (>=)
-- (%>)  = lift2 (>)

-- --Option Contracts
-- european :: Date -> Contract -> Contract
-- european t u = cWhen (at t) (u `cOr` zero)

-- american :: (Date, Date) -> Contract -> Contract
-- american (t1, t2) = anytime (between t1 t2)

-- between :: Date -> Date -> Obs Bool
-- between t1 t2 = lift2 (&&) (date %>= konst t1) (date %<= konst t2)

-- -- american as in comb. paper: american (t1, t2) u 
-- --                               = get (truncate t1 opt) `then` opt
-- --								 where
-- --								    opt :: Contract
-- --								    opt = anytime (perhaps t2 u)







-- --Value Processes

-- --a value process PR is represented as a list of random variables
-- --RV a, with the random variable corresponding to the earliest time
-- --step appearing first in the list.
-- newtype PR a = PR { unPr :: [RV a] } deriving Show


-- --A random variable <code>RV a</code> describes the possible values 
-- --for a value process at a particular time step.  For example, 
-- --the random variable describing the outcome of a dice throw would be 
-- --[1,2,3,4,5,6]. Random variables are therefore implemented as simple lists.
-- type RV a = [a]

-- --takePr</code> truncates a (possibly infinite) value process.
-- takePr :: Int -> PR a -> PR a
-- takePr n (PR rvs) = PR $ take n rvs

-- --horizonPr determines the number of time steps in a value process.
-- horizonPr :: PR a -> Int
-- horizonPr (PR rvs) = length rvs

-- --andPr returns True if every value in a value process is true, false otherwise.
-- andPr :: PR Bool -> Bool
-- andPr (PR rvs) = all and rvs -- and (map and rvs)


-- --Model

-- data Model = Model {
  -- modelStart :: Date,
  -- disc       :: Currency -> (PR Bool, PR Double) -> PR Double,
  -- exch       :: Currency -> Currency -> PR Double,
  -- absorb     :: Currency -> (PR Bool, PR Double) -> PR Double,
  -- rateModel  :: Currency -> PR Double
  -- }

-- exampleModel :: CalendarTime -> Model
-- exampleModel modelDate = Model {
  -- modelStart = (modelDate,0),
  -- disc       = disc,
  -- exch       = exch,
  -- absorb     = absorb,
  -- rateModel  = rateModel
  -- }
  -- where
    
    -- rates :: Double -> Double -> PR Double
    -- rates rateNow delta = PR $ makeRateSlices rateNow 1
      -- where
        -- makeRateSlices rateNow n = rateSlice rateNow n : makeRateSlices (rateNow-delta) (n+1)
        -- rateSlice minRate n = take n [minRate, minRate+(delta*2) ..]
    
    -- rateModels :: [(Currency, PR Double)]
    -- rateModels = [(CHF, rates 7   0.8)
                -- ,(EUR, rates 6.5 0.25)
                -- ,(GBP, rates 8   0.5)
                -- ,(KYD, rates 11  1.2)
				-- ,(USD, rates 5   1)
				-- ,(ZAR, rates 15  1.5)
                                -- ]
    
    -- rateModel :: Currency -> PR Double
    -- rateModel k =
      -- fromMaybe (error $ "rateModel: currency not found " ++ show k)
        -- (lookup k rateModels)
      -- -- case lookup k rateModels of
      -- --   Just x -> x
      -- --   Nothing -> error $ "rateModel: currency not found " ++ (show k)
    
-- --The primitive (disc t k) maps a real-valued random variable at date T,
-- --expressed in currency k, to its "fair" equivalent stochastic(random) value 
-- --process in the same currency k.	
    -- disc :: Currency -> (PR Bool, PR Double) -> PR Double
    -- disc k (PR bs, PR rs) = PR $ discCalc bs rs (unPr $ rateModel k)
      -- where
    
        -- discCalc :: [RV Bool] -> [RV Double] -> [RV Double] -> [RV Double]
        -- discCalc (bRv:bs) (pRv:ps) (rateRv:rs) =
          -- if and bRv -- test for horizon
            -- then [pRv]
            -- else let rest@(nextSlice:_) = discCalc bs ps rs
                     -- discSlice = zipWith (\x r -> x / (1 + r/100)) (prevSlice nextSlice) rateRv
                     -- thisSlice = zipWith3 (\b p q -> if b then p else q) -- allow for partially discounted slices
                                   -- bRv pRv discSlice
                 -- in thisSlice : rest
        
        -- prevSlice :: RV Double -> RV Double
        -- prevSlice [] = []
        -- prevSlice [_] = []
        -- -- prevSlice (_:[]) = []
        -- prevSlice (n1:rest@(n2:_)) = (n1+n2)/2 : prevSlice rest

    -- absorb :: Currency -> (PR Bool, PR Double) -> PR Double
    -- absorb k (PR bSlices, PR rvs) =
      -- PR $ zipWith (zipWith $ \o p -> if o then 0 else p)
                   -- bSlices rvs

    -- exch :: Currency -> Currency -> PR Double
    -- exch k1 k2 = PR (konstSlices 1)
 
-- expectedValue :: RV Double -> RV Double -> Double
-- expectedValue outcomes probabilities = sum $ zipWith (*) outcomes probabilities

-- expectedValuePr :: PR Double -> [Double]
-- expectedValuePr (PR rvs) = zipWith expectedValue rvs probabilityLattice

-- probabilityLattice :: [RV Double]
-- probabilityLattice = probabilities pathCounts
  -- where

    -- probabilities :: [RV Integer] -> [RV Double]
    -- probabilities (sl:sls) = map (\n -> fromInteger n / fromInteger (sum sl)) sl : probabilities sls

    -- pathCounts :: [RV Integer]
    -- pathCounts = paths [1] where paths sl = sl : paths (zipWith (+) (sl++[0]) (0:sl))

-- evalC :: Model -> Currency -> Contract -> PR Double
-- evalC (Model modelDate disc exch absorb rateModel) k = eval    -- punning on record fieldnames for conciseness
  -- where eval Zero           = bigK 0
        -- eval (One k2)       = exch k k2
        -- eval (Give c)       = -(eval c)
        -- eval (o `Scale` c)  = evalO o * eval c
        -- eval (c1 `And` c2)  = eval c1 + eval c2
        -- eval (c1 `Or` c2)   = max (eval c1) (eval c2)
        -- eval (Cond o c1 c2) = condPr (evalO o) (eval c1) (eval c2)
        -- eval (When o c)     = disc   k (evalO o, eval c)
-- --      eval (Anytime o c)  = snell  k (evalO o, eval c)
        -- eval (Until o c)    = absorb k (evalO o, eval c)

-- evalO :: Obs a -> PR a
-- evalO (Obs o) = o time0

-- bigK :: a -> PR a
-- bigK x = PR (konstSlices x)
 
-- konstSlices :: a -> [[a]]
-- konstSlices x = nextSlice [x]
  -- where nextSlice sl = sl : nextSlice (x:sl)
 
-- datePr :: PR Date
-- datePr = PR $ timeSlices [time0]

-- timeSlices :: (Num t1,Enum t1) => [(t, t1)] -> [[(t, t1)]]
-- timeSlices sl@((s,t):_) = sl : timeSlices [(s,t+1) | _ <- [0..t+1]]

-- condPr :: PR Bool -> PR a -> PR a -> PR a
-- condPr = lift3Pr (\b tru fal -> if b then tru else fal)

-- liftPr :: (a -> b) -> PR a -> PR b
-- liftPr f (PR a) = PR $ map (map f) a

-- lift2Pr :: (a -> b -> c) -> PR a -> PR b -> PR c
-- lift2Pr f (PR a) (PR b) = PR $ zipWith (zipWith f) a b

-- lift2PrAll :: (a -> a -> a) -> PR a -> PR a -> PR a
-- lift2PrAll f (PR a) (PR b) = PR $ zipWithAll (zipWith f) a b

-- lift3Pr :: (a -> b -> c -> d) -> PR a -> PR b -> PR c -> PR d
-- lift3Pr f (PR a) (PR b) (PR c) = PR $ zipWith3 (zipWith3 f) a b c

-- zipWithAll :: (a -> a -> a) -> [a] -> [a] -> [a]
-- zipWithAll f (a:as) (b:bs)     = f a b : zipWithAll f as bs
-- zipWithAll f as@(_:_) []       = as
-- zipWithAll f []       bs@(_:_) = bs
-- zipWithAll _ _        _        = []

-- instance Num a => Num (PR a) where
  -- fromInteger i = bigK (fromInteger i)
  -- (+) = lift2PrAll (+)
  -- (-) = lift2PrAll (-)
  -- (*) = lift2PrAll (*)
  -- abs = liftPr  abs
  -- signum = liftPr signum

-- instance Ord a => Ord (PR a) where
  -- max = lift2Pr max

-- instance Eq a => Eq (PR a) where
  -- (PR a) == (PR b) = a == b

-- xm :: Model
-- xm = exampleModel ()

-- evalX :: Contract -> PR Double
-- evalX = evalC xm USD

-- zcb :: Date -> Double -> Currency -> Contract
-- zcb t x k = cWhen (at t) (scale (konst x) (one k))

-- c1 :: Contract
-- c1 = zcb t1 10 USD

-- t1 :: Date
-- t1 = mkDate t1Horizon

-- t1Horizon :: TimeStep
-- t1Horizon = 3

-- c11 :: Contract
-- c11 = european (mkDate 2)
        -- (zcb (mkDate 20) 0.4 USD `cAnd`
        -- zcb (mkDate 30) 9.3 USD `cAnd`
        -- zcb (mkDate 40) 109.3 USD `cAnd`
        -- give (zcb (mkDate 12) 100 USD))

-- pr1 :: PR Double
-- pr1 = evalX c1

-- tr1 :: [RV Double]
-- tr1 = unPr pr1

-- absorbEx :: Date -> Double -> Currency -> Contract
-- absorbEx t x k = cUntil (konst t %> date) (scale (konst x) (one k))