module Simulation where

import NewObs
import Test
import System.Environment
import System.IO
 

--input data


-- =========================================================================================================
--                          ESCENARIO -I The Weather Hedge
-- =========================================================================================================
-- A local power company could see its bottom line negatively affected if the winter is unseasonably warm, 
-- thereby lowering the demand for heating oil and gas. The power company could hedge against this outcome 
-- by purchasing a contract that would pay out cash if the temperature averaged five degrees above the 
-- historical average for the duration of winter. 
-- On the other side of that trade could be a hedge fund that was betting on the seasonal patterns,
-- rather than the unseasonable.

--Hedge contract. Power Company
weatherContractR = cWhen (At (O("",C(2019,1,2))) ) 
                            (cond (IsTrue (O("Hotter Winter", True)))
                                          (scale (O("Petrol costs compensation" , 1000000)) (One EUR))
                                          (zero)
                            )
--The other side of the hedge, the Bank or other financial institution willing to take the trade for a premium.
weatherContractP = cWhen (At (O("",C(2019,1,2))) ) 
                            (cond (IsTrue (O("Hotter Winter", True)))
                                          ( give(
                                                 (scale (O("Petrol costs compensation" , 1000000)) (One EUR)) ) 
                                          )
                                          (zero)
                            )
--

-- =======_Observables_=========

--list of observable bool values
boolObs = [  (Day 0, [])
             , ((Day 1), [O("Hotter Winter",True), O("Average Winter",False) ] )
             , ((Day 2), [O("Hotter Winter",False), O("Average Winter",True) ] )
             , ((Day 3), [O("Hotter Winter",True), O("Average Winter",False) ] )
            ]

--start date (Day)
today = C(2019,1,1)


--BARE BONES SIMULATOR--
sim1 [] c = []
sim1 boolObs c = case c of
                            When (At t) u | (incrementDate today (fst(head(boolObs)))) == (valObs t) 
                                                           -> activateContract (incrementDate today (fst(head(boolObs)))) (snd (head(boolObs))) u
                                          | otherwise -> (tlog (incrementDate today (fst(head(boolObs)))) [] [] ): (sim1 (tail boolObs) c)
                            Empty -> (tlog (incrementDate today (fst(head(boolObs)))) [] [Empty] ): (sim1 (tail boolObs) Empty)
--
-- Once the main clause of a contract is activated, look inside for other clauses												  

activateContract date obs c = case c of
                         Cond (IsTrue o) u1 u2 | valObs(matchContractToObs o obs) == True
                                                                  -> ( tlog  date [o]  [u1] ): sim1 (tail boolObs) Empty
                                               | otherwise -> ( tlog  date [matchContractToObs o obs]  [u2] ) : (sim1 (tail boolObs) Empty)
--

-- Checks fot the right obsevable in the list
matchContractToObs o [] = (O(nameObs o, False))
matchContractToObs o obs | eqComp o (head obs) = (head obs)
                         | otherwise = matchContractToObs o (tail obs)
--


--OUTPUT 
tlog a b [] = (a ,b, [])
tlog a b c = (a, b, c)




















-- tlog : sim (today+1) (tail boolobs) (tail doubleobs) contract'
-- = -- do something with today, heads of obs-lisrs, and contract
   -- to get a transaction log for today  (tlog),
   -- and poss. modifed contract'
 

 
 
 -- Sim output data
 -- transaction log = list of dated events

-- [ (1, [], c0)
-- , (2, [PAY CHF 1000], c1)
-- , (3, [], c1)
-- , (4, [PAY CHF 1000, RECEIVE USD 3000], c2)
-- , (5, [CHOSE OPTION "A"], c3)
-- , ...
-- ]

-- Also list of ofs double values
-- [ ( Day   - ordered by Day - start with start day
  -- , [Obs Double] -- all double observabel values for that day,
  -- )
-- ]

-- [ ( 1, [O("X",3.0), O("Y",2.0), O("P",100000.0) ] )
-- , ( 2, [O("X",3.1), O("Y",21.9), O("P",100000.0) ] )
-- , ( 3, [O("X",3.15), O("Y",20.2), O("P",100000.0) ] )
-- , ....
-- ]
-- Pro tip !  use 'replicate' to generate long streches were nothing changes
--tlog = ( Day, [Transaction], Contract)
 
 
-- transactionLog = do
                -- outh <- openFile "tiptop.txt" WriteMode
                -- hPutStrLn outh "Try This Out\nand This too\n" --have a function here 
                -- hClose outh


  