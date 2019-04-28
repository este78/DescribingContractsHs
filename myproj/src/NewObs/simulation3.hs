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
sim3 ((day,obs):dObs) c = 
             case c of
                When (At t) u | day == (valObs t)  
                                  ->  activateContract ((day,obs):dObs) u
                              | otherwise 
                                  -> (tlog'' day [] [] ) : (sim3 dObs c)

                Empty -> (tlog'' day [] [Empty] ) : (sim3 dObs Empty)
--
-- Once the main clause of a contract is activated, 
-- look inside for other clauses												  
activateContract ((day,obs):dObs) c = 
             case c of
               Or u1 u2 
                    |valObs(head obs) == True
                            -> ( tlog''  day [head obs]  [u1] ): sim3 dObs u1
                    |otherwise
                            -> ( tlog'' day [head obs] [u2] ): sim3 dObs u2
               
               Scale o1 u 
                     -> (tlog'' day [] [scale o1 u] ) : sim3 dObs Empty

               Zero   -> ( tlog'' day [] [zero] ) : sim3 dObs Empty
--

--

--
tlog'' a b c = (a,b,c)

dObs = [ ((Day 1),[O("Owner Chooses Contract", False)] )
        , ((Day 2),[O("Owner Chooses Contract", False)] )
        , ((Day 3),[O("Owner Chooses Contract", True)] )
        , ((Day 4),[O("Owner Chooses Contract", False)] )
        , ((Day 5),[O("Owner Chooses Contract", False)] )
        , ((Day 6),[O("Owner Chooses Contract", False)] )
        , ((Day 7),[O("Owner Chooses Contract", False)] )
        , ((Day 8),[O("Owner Chooses Contract", False)] )
        , ((Day 9),[O("Owner Chooses Contract", False)] )
       ]