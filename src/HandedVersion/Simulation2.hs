module Simulation2 where


import NewObs
import CombDate
import Combinators
import Printing
import Simulation

-- ============================================================================
--                           The Contract
-- ============================================================================
-- Car loan €24,000 payable in 12 "monthly" installments, Variable 0.075 APR   
microsoft = 
            (cWhen (At(O(" ", Day 0))) (give(scale 
                                        (O("LIBOR 3 month", 100000000)) 
                                                     (one EUR))
                                       )
            )
           `cAnd`
              ((cWhen $ At (O(" ", Day 4)) ) (give(                        
                                               (scale (O("LIBOR 3 month" , 100000000)) 
                                                                      (One EUR) )
                                              )
                                         )
              )
           `cAnd` 
              ((cWhen $ At (O(" ", Day 8)) ) (give (
                                               (scale (O("LIBOR 3 month" , 100000000)) 
                                                                     (One EUR))
                                                   )  
                                             )
              )
           `cAnd` 
              ((cWhen $ At (O(" ", Day 12)) ) (give (
                                               (scale (O("LIBOR 3 month" , 100000000)) 
                                                                     (One EUR))
                                                   )  
                                             )
              )
           `cAnd` 
              ((cWhen $ At (O(" ", Day 16)) ) (give (
                                               (scale (O("LIBOR 3 month" , 100000000)) 
                                                                     (One EUR))
                                                   )  
                                             )
              )
           `cAnd` 
              ((cWhen $ At (O(" ", Day 20)) ) (give (
                                               (scale (O("LIBOR 3 month" , 100000000)) 
                                                                     (One EUR))
                                                   )  
                                             )
              )
           `cAnd` 
              ((cWhen $ At (O(" ", Day 24)) ) (give (
                                               (scale (O("LIBOR 3 month" , 100000000)) 
                                                                     (One EUR))
                                                   )  
                                             )
              )
-- =====================================================================================
intel =			   
             ((cWhen $ At (O(" ", Day 0)) ) (give (
                                               (scale (O("Fixed Rate 2.5%" , 100000000)) 
                                                                     (One EUR))
                                                   )  
                                             )
              )
           `cAnd` 
              ((cWhen $ At (O(" ", Day 4)) ) (give (
                                               (scale (O("Fixed Rate 2.5%" , 100000000)) 
                                                                     (One EUR))
                                                   )  
                                             )
              )
           `cAnd` 
              ((cWhen $ At (O(" ", Day 8)) ) (give (
                                               (scale (O("Fixed Rate 2.5%" , 100000000)) 
                                                                     (One EUR))
                                                   )  
                                             )
              )   
           `cAnd` 
              ((cWhen $ At (O(" ", Day 12)) ) (give (
                                               (scale (O("Fixed Rate 2.5%" , 100000000)) 
                                                                     (One EUR))
                                                   )  
                                             )
              )
           `cAnd` 
              ((cWhen $ At (O(" ", Day 16)) ) (give (
                                               (scale (O("Fixed Rate 2.5%" , 100000000)) 
                                                                     (One EUR))
                                                   )  
                                             )
              )
           `cAnd` 
              ((cWhen $ At (O(" ", Day 20)) ) (give (
                                               (scale (O("Fixed Rate 2.5%" , 100000000)) 
                                                                     (One EUR))
                                                   )  
                                             )
              )
		   `cAnd` 
              ((cWhen $ At (O(" ", Day 24)) ) (give (
                                               (scale (O("Fixed Rate 2.5%" , 100000000)) 
                                                                     (One EUR))
                                                   )  
                                             )
              )

--

-- =======_BARE BONES SIMULATOR_============
--The sim function looks into the primitives that form the contract. 
--This is bare bones and deals with the contracts detailed above
sim2 [] c = []
sim2 ((day,obs):doubObs) c = 
                   case c of
                       And u1 u2 -> simAnd ((day,obs):doubObs) [u1,u2]
                       When (At t) u | day == (valObs t) 
                                                     -> (actContract doubObs u)
                                     | otherwise     -> (tlog' day [] [] )
                                                             : (sim2 doubObs c)
                       Scale o u -> (tlog' day [pair2Obs o obs] 
                                       [(scale (pair2Obs o obs) u)]
                                    )  : (sim2 doubObs Zero)
                       Zero -> ( tlog' day [] [Zero] )
                                         : (sim2 doubObs Zero)
--
-- -- Once the main clause of a contract is activated, look inside for other clauses												  
actContract [] c = []
actContract ((day,obs):doubObs) c = 
              case c of
                   And u1 u2 -> simAnd ((day,obs):doubObs) [u1,u2]
                   Give u -> (tlog' day [head obs] [u]) : (sim2 doubObs Zero) 
                   Scale o u -> (tlog' day [pair2Obs o obs] 
                                       [(scale (pair2Obs o obs) u)]
                                 )  : (sim2 doubObs Zero)
--
-- Checks fot the right obsevable in the list
pair2Obs o [] = o
pair2Obs o (ob:obs) | (nameObs o) ==  "LIBOR 3 month" 
                                   = O("1 payment at LIBOR " ++ show (valObs ob)++" ",
                                      ( (valObs ob) * (valObs o) )
                                      )
                    | (nameObs o) == "Fixed Rate 2.5%"
                                   = O("Fixed Rate 2.5%", (valObs ob) * (valObs o))
                    | otherwise = pair2Obs o obs
--
-- Examines And Clauses, they can get really complicated 
-- 
simAnd doubObs [] = sim2 doubObs Zero
simAnd ((day,obs):doubObs) (c1:cs) = 
           
         case c1 of
            And u1 u2 -> simAnd ((day,obs):doubObs) (u1:u2:cs)
  
            When (At t) u |day == (valObs t)-> (actContractAnd 
                                                    ((day,obs):doubObs) (u:cs) ) 
                          |otherwise        -> (tlog' day [] [] )
                                                     :(simAnd doubObs (c1:cs))
            Give u -> (tlog' day [] [give(c2Obs u obs)]) : (simAnd 
                                                      (doubObs) cs )
            Zero -> ( tlog' day [] [Zero] )  :  (sim2 doubObs Zero)
--
--Examines deeper clauses in the contract for processing
--
actContractAnd ((day,obs):doubObs) [] = (tlog' day [] [Zero]) 
                                                         : sim2 doubObs Zero 
actContractAnd ((day,obs):doubObs) (u1:us) = 
         case u1 of
            And u11 u22 -> actContractAnd ((day,obs):doubObs) (u11:u22:us)
            
            When (At t) u |day == (valObs t)-> (actContractAnd 
                                                    ((day,obs):doubObs) (u:us) ) 
                          |otherwise        -> (tlog' day [] [] )
                                                       :(simAnd doubObs (u1:us))
            
            Give u -> (tlog' day [] [give(c2Obs u obs)]) : (simAnd 
                                                   (doubObs) us )  
            
            Scale o u -> (tlog' day [pair2Obs o obs] 
                               [(scale (pair2Obs o obs) u)]
                         ) : (simAnd doubObs us)
-- 
-- Standalone give U contract
c2Obs u obs = case u of Scale o u1 ->scale (pair2Obs o obs) u1


-- =======_OUTPUT_============================================================ 
tlog' a [] c = (a,[],c)
tlog' a b [] = (a ,b, [])
tlog' a b c = (a, b, c)


--list of observable bool obs
doubObs = [  
               ((Day 0), [O("LIBOR 3 month", 0.075), O("Fixed Rate 2.5%", 0.025), O("Principal", 24000)] )
             , ((Day 1),  [O("LIBOR 3 month", 0.075), O("Fixed Rate 2.5%", 0.025), O("Principal", 24000)] )
             , ((Day 2),  [O("LIBOR 3 month", 0.075), O("Fixed Rate 2.5%", 0.025), O("Principal", 24000)] )
             , ((Day 3),  [O("LIBOR 3 month", 0.075), O("Fixed Rate 2.5%", 0.025), O("Principal", 24000)] )
             , ((Day 4),  [O("LIBOR 3 month", 0.075), O("Fixed Rate 2.5%", 0.025), O("Principal", 24000)]  )
             , ((Day 5),  [O("LIBOR 3 month", 0.075), O("Fixed Rate 2.5%", 0.025), O("Principal", 24000)] )
             , ((Day 6),  [O("LIBOR 3 month", 0.075), O("Fixed Rate 2.5%", 0.025), O("Principal", 24000)] )
             , ((Day 7),  [O("LIBOR 3 month", 0.075), O("Fixed Rate 2.5%", 0.025), O("Principal", 24000)] )
             , ((Day 8),  [O("LIBOR 3 month", 0.075), O("Fixed Rate 2.5%", 0.025), O("Principal", 24000)]  )
             , ((Day 9),  [O("LIBOR 3 month", 0.075), O("Fixed Rate 2.5%", 0.025), O("Principal", 24000)] )
             , ((Day 10), [O("LIBOR 3 month", 0.075), O("Fixed Rate 2.5%", 0.025), O("Principal", 24000)] )
             , ((Day 11), [O("LIBOR 3 month", 0.075), O("Fixed Rate 2.5%", 0.025), O("Principal", 24000)] )
             , ((Day 12), [O("LIBOR 3 month", 0.060), O("Fixed Rate 2.5%", 0.025), O("Principal", 24000)]  )
             , ((Day 13), [O("LIBOR 3 month", 0.060), O("Fixed Rate 2.5%", 0.025), O("Principal", 24000)] )
             , ((Day 14), [O("LIBOR 3 month", 0.060), O("Fixed Rate 2.5%", 0.025), O("Principal", 24000)] )
             , ((Day 15), [O("LIBOR 3 month", 0.060), O("Fixed Rate 2.5%", 0.025), O("Principal", 24000)] )
             , ((Day 16), [O("LIBOR 3 month", 0.060), O("Fixed Rate 2.5%", 0.025), O("Principal", 24000)]  )
             , ((Day 17), [O("LIBOR 3 month", 0.075), O("Fixed Rate 2.5%", 0.025), O("Principal", 24000)] )
             , ((Day 18), [O("LIBOR 3 month", 0.075), O("Fixed Rate 2.5%", 0.025), O("Principal", 24000)] )
             , ((Day 19), [O("LIBOR 3 month", 0.075), O("Fixed Rate 2.5%", 0.025), O("Principal", 24000)] )
             , ((Day 20), [O("LIBOR 3 month", 0.085), O("Fixed Rate 2.5%", 0.025), O("Principal", 24000)]  )
             , ((Day 21), [O("LIBOR 3 month", 0.085), O("Fixed Rate 2.5%", 0.025), O("Principal", 24000)] )
             , ((Day 22), [O("LIBOR 3 month", 0.085), O("Fixed Rate 2.5%", 0.025), O("Principal", 24000)] )
             , ((Day 23), [O("LIBOR 3 month", 0.085), O("Fixed Rate 2.5%", 0.025), O("Principal", 24000)] )
             , ((Day 24), [O("LIBOR 3 month", 0.085), O("Fixed Rate 2.5%", 0.025), O("Principal", 24000)]  )
             , ((Day 25), [O("LIBOR 3 month", 0.075), O("Fixed Rate 2.5%", 0.025), O("Principal", 24000)] )
             , ((Day 26), [O("LIBOR 3 month", 0.075), O("Fixed Rate 2.5%", 0.025), O("Principal", 24000)] )
             , ((Day 27), [O("LIBOR 3 month", 0.075), O("Fixed Rate 2.5%", 0.025), O("Principal", 24000)] )
             , ((Day 28), [O("LIBOR 3 month", 0.075), O("Fixed Rate 2.5%", 0.025), O("Principal", 24000)]  )
             , ((Day 29), [O("LIBOR 3 month", 0.075), O("Fixed Rate 2.5%", 0.025), O("Principal", 24000)] )
             , ((Day 30), [O("LIBOR 3 month", 0.075), O("Fixed Rate 2.5%", 0.025), O("Principal", 24000)] )
          ]
--






-- ================ Old Code =====================================
-- simG [] c = []
-- simG ((day,obs):doubObs) c = case c of
                          -- And u1 u2 -> simAnd ((day,obs):doubObs) [u1,u2]
                          -- When (At t) u | day == (valObs t) 
                                                 -- -> actContract 
                                                             -- doubObs u
                                       -- | otherwise 
                                              -- -> (tlog' day [] [] )
                                                     -- : (simG doubObs c)
                          -- Scale o u -> (tlog' 
                                             -- day [pair2Obs o obs] 
                                             -- [(give $ 
                                                 -- scale (pair2Obs o obs) u)]
                                               -- )
                                              -- : (sim2 doubObs Zero)

-- --

 -- `cAnd` 
              -- ((cWhen $ At (O(" ", Day 32)) ) (give (
                                               -- (scale (O("Variable APR" , 2000)) 
                                                                     -- (One EUR))
                                                   -- )  
                                             -- )
              -- )
           -- `cAnd` 
              -- ((cWhen $ At (O(" ", Day 36)) ) (give (
                                               -- (scale (O("Variable APR" , 2000)) 
                                                                     -- (One EUR))
                                                   -- )  
                                             -- )
              -- )   
           -- `cAnd` 
              -- ((cWhen $ At (O(" ", Day 40)) ) (give (
                                               -- (scale (O("Variable APR" , 2000)) 
                                                                     -- (One EUR))
                                                   -- )  
                                             -- )
              -- )
           -- `cAnd` 
              -- ((cWhen $ At (O(" ", Day 44)) ) (give (
                                               -- (scale (O("Variable APR" , 2000)) 
                                                                     -- (One EUR))
                                                   -- )  
                                             -- )
              -- )
           -- `cAnd` 
              -- ((cWhen $ At (O(" ", Day 48)) ) (give (
                                               -- (scale (O("Variable APR" , 2000)) 
                                                                     -- (One EUR))
                                                   -- )  
                                             -- )
              -- )
-- swapContract' = 
              -- ((cWhen $ At (O(" ", Day 4)) ) 
                                             -- (give(                        
                                                        -- (scale (O("Libor" , 100000000)) 
                                                                    -- (One EUR) )
                                                        -- )
                                                      -- )
                                                     -- (scale (O("Fixed rate" , 100000000)) 
                                                                          -- (One EUR) )
                                             -- )
           -- `cAnd` 
              -- ((cWhen $ At (O(" ", Day 8)) ) (cAnd   (give (
                                                            -- (scale (O("Libor 3 month" , 100000000)) 
                                                                          -- (One EUR))
                                                           -- )  
                                                     -- )
                                                     -- (scale (O("Fixed rate" , 100000000)) 
                                                                          -- (One EUR) )
                                             -- )
              -- )
           -- `cAnd` 
              -- ((cWhen $ At (O(" ", Day 12)) ) (cAnd (give (
                                                          -- (scale (O("Variable APR" ,  100000000)) 
                                                                                -- (One EUR))
                                                          -- )  
                                                    -- )
                                                     -- (scale (O("Fixed rate" , 100000000)) 
                                                                          -- (One EUR) )													
                                              -- )
              -- )
            -- `cAnd`
              -- ((cWhen $ At (O(" ", Day 16)) ) (cAnd (give (
                                                          -- (scale (O("LIBOR 3 month" ,  100000000)) 
                                                                                -- (One EUR))
                                                          -- )  
                                                    -- )
                                                     -- (scale (O("Fixed rate" , 100000000)) 
                                                                          -- (One EUR) )													
                                              -- )
              -- )
            -- `cAnd`
              -- ((cWhen $ At (O(" ", Day 20)) ) (cAnd (give (
                                                          -- (scale (O("LIBOR 3 month" ,  100000000)) 
                                                                                -- (One EUR))
                                                          -- )  
                                                    -- )
                                                     -- (scale (O("Fixed rate" , 100000000)) 
                                                                          -- (One EUR) )													
                                              -- )
              -- )
           -- `cAnd` 
              -- ((cWhen $ At (O(" ", Day 24)) ) (cAnd (give (
                                                          -- (scale (O("LIBOR 3 month" ,  100000000)) 
                                                                                -- (One EUR))
                                                          -- )  
                                                    -- )
                                                     -- (scale (O("Fixed rate 2.5%" ,100000000)) 
                                                                          -- (One EUR) )													
                                              -- )
              -- )           
           -- `cAnd` 
              -- ((cWhen $ At (O(" ", Day 28)) ) (cAnd (give (
                                                          -- (scale (O("LIBOR 3 month" ,100000000)) 
                                                                                -- (One EUR))
                                                          -- )  
                                                    -- )
                                                     -- (scale (O("Fixed rate" , 100000000)) 
                                                                          -- (One EUR) )													
                                              -- )
              -- )
          