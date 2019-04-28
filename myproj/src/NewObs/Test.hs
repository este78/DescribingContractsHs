module Test where


import NewObs


libor t | t >= (Day 13) = O("on a Principal of 1000000 a",0.0095338)
        | t >= (Day 12) = O("on a Principal of 1000000 a",0.0098675)
        | t >= (Day 11) = O("on a Principal of 1000000 a",0.0103025)
        | t >= (Day 10) = O("on a Principal of 1000000 a",0.0103125)
        | t >= (Day 9) = O("on a Principal of 1000000 a",0.0101225)
        | t >= (Day 8) = O("on a Principal of 1000000 a",0.0094600)
        | t >= (Day 7) = O("on a Principal of 1000000 a",0.0090238)
        | t >= (Day 6) = O("on a Principal of 1000000 a",0.0089400)
        | t >= (Day 5) = O("on a Principal of 1000000 a",0.0090531)
        | t >= (Day 4) = O("on a Principal of 1000000 a",0.0078931)
        | t >= (Day 3) = O("on a Principal of 1000000 a",0.0072535)
        | t >= (Day 2) = O("on a Principal of 1000000 a",0.0080688)
        | t >= (Day 1) = O("on a Principal of 1000000 a", 0.0080)
        | otherwise  = O("on a Principal of 1000000 a", 0.0)
--
priceToday s t | s == "Oil(Brent)" && t == (Day 92) = O(s ++ show t,0.47)
               | s == "Oil(Brent)" && t == (Day 92) = O(s ++ show t,0.48)
               | s == "Oil(Brent)" && t == (Day 92) = O(s ++ show t,0.48)
               | s == "Oil(Brent)" && t == (Day 92) = O(s ++ show t,0.49)
               | s == "Gold gr." && t == (Day 92) = O(s ++ show t,41.03)
               | s == "Gold gr." && t == (Day 92) = O(s ++ show t,41.23)
               | s == "Gold gr." && t == (Day 92) = O(s ++ show t,40.95)
               | s == "Gold gr." && t == (Day 92) = O(s ++ show t,41.5)
               |otherwise = O(s ++ show t , 0.0)
--
identify t r | nameObs r == "LIBOR" = libor (  t - (valObs r)  )
             | otherwise = libor (  t - (valObs r)   )
--

interestSwapFlt t r  = (scale  (  identify t r  )  (One  (Rate "LIBOR")) )  
--
interestSwapFxd t fxrate = (  scale  (  fxrate )  (One  (Rate "FIXED")) )
--
swapContract  = cWhen ( At  ( O(" ", (Day 91)) ) ) (
                       interestSwapFlt (Day 91) (O("LIBOR",(Day 90)))
                       `cAnd` give (
                                    interestSwapFxd (Day 91) (O("on a Principal of 100000 a", 0.01))
                                   ) 
                )
                `cAnd` cWhen ( At  ( O(" ", (Day 92)) ) )(
                             interestSwapFlt (Day 92) (O("LIBOR",(Day 90)))
                            `cAnd` give (
                                         interestSwapFxd (Day 92) (O("on a Principal of 1000000 a", 0.01))
                                        )
                      )
                `cAnd` cWhen ( At  ( O(" ", (Day 93)) ) )(
                             interestSwapFlt (Day 93) (O("LIBOR",(Day 90)))
                `            cAnd` give (
                                         interestSwapFxd (Day 93) (O("on a Principal of 1000000 a", 0.01))
                                        ) 
                       )
                `cAnd` cWhen ( At  ( O(" ", (Day 94)) ) )(
                              interestSwapFlt (Day 94) (O("LIBOR",(Day 90)))
                              `cAnd` give (
                                           interestSwapFxd (Day 94) (O("on a Principal of 1000000 a", 0.01))
                                          ) 
                       )
                `cAnd` cWhen ( At  ( O(" ", (Day 95)) ) )(
                             interestSwapFlt (Day 95) (O("LIBOR",(Day 90)))
                `            cAnd` give (
                                         interestSwapFxd (Day 95) (O("on a Principal of 1000000 a", 0.01))
                                        ) 
                       )
--
europeanContract = european ( O("Strike Date", (Day 10)) ) 
                                         (give
                                             (fwd (O(" ",Day 20)) (O(" ", 1000))  USD )
                                         )
--
carLoan' = cWhen (At(O(" ", Day 1)))(scale (O("Principal", 24000)) (one EUR))      --u1
         `cAnd`
              (give (                                                             --u2
                      (cWhen $ At (O(" ", Day 8)) )
                         (scale (O("Variable APR" , 2000)) 
                            (One EUR)
                         )
                    )
              )

-- ORIGINAL SWAP CONTRACT
-- interestSwapFlt t principal r  = (scale  (  lift2 (*) principal (identify t r)  )
                                        -- (One  CHF)
                                 -- ) 
-- --
-- interestSwapFxd t principal fxrate = (  scale  (lift2 (*)  principal  fxrate)
                                                    -- (One  CHF)
                                     -- ) 
-- --
-- swapContract  = cWhen ( at  (C(2019,1,10)) ) (
                       -- interestSwapFlt (C(2019,1,10)) (O("Principal", 1000000))  (O("LIBOR",(Day 90)))
                       -- `cAnd` give (
                                    -- interestSwapFxd (C(2019,1,10)) (O("Principal", 1000000))  (O("Fixed Rate", 0.01))
                                   -- ) 
                -- )
                -- `cAnd` cWhen (at (C(2019,4,10)))(
                             -- interestSwapFlt (C(2020,4,10)) (O("Principal", 1000000))  (O("LIBOR",(Day 90)))
                            -- `cAnd` give (
                                         -- interestSwapFxd (C(2020,4,10)) (O("Principal", 1000000))  (O("Fixed Rate", 0.01))
                                        -- )
                      -- )
                -- `cAnd` cWhen (at (C(2019,7,10)))(
                             -- interestSwapFlt (C(2020,7,10)) (O("Principal", 1000000))  (O("LIBOR",(Day 90)))
                -- `            cAnd` give (
                                         -- interestSwapFxd (C(2020,7,10)) (O("Principal", 1000000))  (O("Fixed Rate", 0.01))
                                        -- ) 
                       -- )
                -- `cAnd` cWhen (at (C(2019,10,10)))(
                              -- interestSwapFlt (C(2020,10,10)) (O("Principal", 1000000))  (O("LIBOR",(Day 90)))
                              -- `cAnd` give (
                                           -- interestSwapFxd (C(2020,10,10)) (O("Principal", 1000000))  (O("Fixed Rate", 0.01))
                                          -- ) 
                       -- )
                -- `cAnd` cWhen (at (C(2020,1,10)))(
                             -- interestSwapFlt (C(2020,1,10)) (O("Principal", 1000000))  (O("LIBOR",(Day 90)))
                -- `            cAnd` give (
                                         -- interestSwapFxd (C(2020,1,10)) (O("Principal", 1000000))  (O("Fixed Rate", 0.01))
                                        -- ) 
                       -- )
