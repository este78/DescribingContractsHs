module Simulation3 where

import CombDate
import NewObs
import Combinators
import Printing

eurOption = 
       european (At (O("Strike Date", Day 3)) ) 
                       (
                         (fwd (At(O(" ",Day 7))) (O(" ", 1000))  USD )
                       )
--


--The sim function looks into the primitives that form the contract. 
--This is bare bones and deals with the contracts detailed above
sim3 [] c = []
sim3 ((day,obs):bObs) c = 
             case c of
                When (At t) u | day == (valObs t)  
                                  ->  activateContract ((day,obs):bObs) u
                              | otherwise 
                                  -> (tlog'' day [] [] ) : (sim3 bObs c)

                Zero -> (tlog'' day [] [Zero] ) : (sim3 bObs Zero)
--
-- Once the main clause of a contract is activated, 
-- look inside for other clauses												  
activateContract ((day,obs):bObs) c = 
             case c of
               Or u1 u2 
                    |valObs(head obs) == True
                            -> ( tlog''  day [head obs]  [u1] ): sim3 bObs u1
                    |otherwise
                            -> ( tlog'' day [head obs] [u2] ): sim3 bObs u2
               
               Scale o1 u 
                     -> (tlog'' day [] [scale o1 u] ) : sim3 bObs Zero

               Zero   -> ( tlog'' day [] [zero] ) : sim3 bObs Zero
--

--

--
tlog'' a b c = (a,b,c)

bObs = [ ((Day 1),[O("Owner Chooses Contract", False)] )
        , ((Day 2),[O("Owner Chooses Contract", False)] )
        , ((Day 3),[O("Owner Chooses Contract", True)] )
        , ((Day 4),[O("Owner Chooses Contract", False)] )
        , ((Day 5),[O("Owner Chooses Contract", False)] )
        , ((Day 6),[O("Owner Chooses Contract", False)] )
        , ((Day 7),[O("Owner Chooses Contract", False)] )
        , ((Day 8),[O("Owner Chooses Contract", False)] )
        , ((Day 9),[O("Owner Chooses Contract", False)] )
       ]

-- sim4 [] a = []
-- sim4 _ [] = []
-- sim4 (day,(bools,dobs):observables) (c:contracts) =
                 -- case c of
                 -- Zero -> (tlog'' day [] [Zero]) : sim4 observables contracts

                 -- Scale o u -> scaling (day,(bools,dobs):observables) o [u]
-- --




-- scaling (day,(bools,dobs):observables) o (u:us) contracts = 
       -- case u of 
       -- Zero -> (tlog'' day [] [Zero]) : sim4 observables contracts
       -- Scale o1 u 
          -- case u of One -> (tlog'' day [o1] [scale ((valObs o)*(valObs o1)) u] ) : sim4 (day,(bools,dobs):observables) contracts
                    -- otherwise ->