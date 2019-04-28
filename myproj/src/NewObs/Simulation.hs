module Simulation where

import NewObs
import CombDate
import Combinators
import Printing

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
wContractR = 
      cWhen (At (O("", Day 3)) ) 
                (cond (IsTrue (O("Hotter Winter", True)))
                             (scale (O("Petrol costs compensation" , 1000000)) 
                                                                     (One EUR)
                              )
                              (zero)
                )
--The other side of the hedge, the Bank or other financial institution willing to take the trade for a premium.
wContractP = 
      cWhen (At (O("", Day 3)) ) 
                (cond (IsTrue (O("Hotter Winter", True)))
                        ( give(
                               (scale (O("Petrol costs compensation" , 1000000)) 
                                                                     (One EUR))
                              ) 
                        )
                        (zero)
                )
--

-- =======_Observables_=========

--list of observable bool values
boolObs = [  (Day 0, [])
             , ((Day 1), [O("Hotter Winter",False), O("Average Winter",True) ] )
             , ((Day 2), [O("Hotter Winter",False), O("Average Winter",True) ] )
             , ((Day 3), [O("Hotter Winter",True), O("Average Winter",False) ] )
             , ((Day 4), [] )
             , ((Day 5), [O("Hotter Winter",False), O("Average Winter",True) ] )
             , ((Day 6), [] )
            ]


--calendar t = incrementDate t (Day 1) 
                         

-- =======_BARE BONES SIMULATOR_============
--The sim function looks into the primitives that form the contract. 
--This is bare bones and deals with the contracts detailed above
sim1 [] c = []
sim1 ((today,values):boolObs) c = 
             case c of
                When (At t) u | today == (valObs t)  
                                  ->  activateContract today values u
                              | otherwise 
                                  -> (tlog today [] [] ) : (sim1 boolObs c)

                Empty -> (tlog today [] [Empty] ) : (sim1 (tail boolObs) Empty)
--
-- Once the main clause of a contract is activated, 
-- look inside for other clauses												  
activateContract today values c = 
             case c of
               Cond (IsTrue o) u1 u2 
                    | valObs(matchContractToObs o values) == True
                       -> ( tlog  today [o]  [u1] ): sim1 (tail boolObs) Empty
                    | otherwise
                       -> ( tlog  today [matchContractToObs o values]  [u2] )
                                                : (sim1 (tail boolObs) Empty)
--

-- Checks for the right obsevable in the list, tor produce 
matchContractToObs o [] = (O(nameObs o, False))
matchContractToObs o (ob:obs) | eqComp o ob = ob
                              | otherwise = matchContractToObs o obs
--


-- =======_OUTPUT_================ 
tlog a b [] = (a ,b, [])
tlog a b c = (a, b, c)























-- Leftover Code

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

