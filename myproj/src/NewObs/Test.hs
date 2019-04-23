module Test where


import NewObs

libor t | t >= C(2021,10,01) = O("LIBOR 3month",0.0095338)
        | t >= C(2021,07,01) = O("LIBOR 3month",0.0098675)
        | t >= C(2021,04,01) = O("LIBOR 3month",0.0103025)
        | t >= C(2021,01,01) = O("LIBOR 3month",0.0103125)
        | t >= C(2020,10,01) = O("LIBOR 3month",0.0101225)
        | t >= C(2020,07,01) = O("LIBOR 3month",0.0094600)
        | t >= C(2020,04,01) = O("LIBOR 3month",0.0090238)
        | t >= C(2020,01,01) = O("LIBOR 3month",0.0089400)
        | t >= C(2019,10,01) = O("LIBOR 3month",0.0090531)
        | t >= C(2019,07,01) = O("LIBOR 3month",0.0078931)
        | t >= C(2019,04,01) = O("LIBOR 3month",0.0072535)
        | t >= C(2019,01,01) = O("LIBOR 3month",0.0080688)
--

--
identify t r | nameObs r == "LIBOR" = libor (  decrementDate t (valObs r)  )
             | otherwise = libor (  decrementDate t (valObs r)  )
--

interestSwapFlt t principal r  = cWhen t 
                                (scale  (  lift2 (*) principal (identify t r)  )
                                        (One  CHF)
                                 ) 
--
interestSwapFxd t principal fxrate = cWhen t 
                                          (  scale  (lift2 (*)  principal  fxrate)
                                                    (One  CHF)
                                          ) 
--
testContract2 = interestSwapFlt (C(2019,10,10)) (O("Principal", 1000000))  (O("LIBOR",(Day 90)))
                `cAnd` give (
                             interestSwapFxd (C(2019,10,10)) (O("Principal", 1000000))  (O("Fixed Rate", 0.01))
                            )
                `cAnd`
							interestSwapFlt (C(2020,1,10)) (O("Principal", 1000000))  (O("LIBOR",(Day 90)))
                `cAnd` give (
                             interestSwapFxd (C(2020,1,10)) (O("Principal", 1000000))  (O("Fixed Rate", 0.01))
                            )
				`cAnd`			
                            interestSwapFlt (C(2020,4,10)) (O("Principal", 1000000))  (O("LIBOR",(Day 90)))
                `cAnd` give (
                             interestSwapFxd (C(2020,4,10)) (O("Principal", 1000000))  (O("Fixed Rate", 0.01))
                            )
                `cAnd`
                            interestSwapFlt (C(2020,7,10)) (O("Principal", 1000000))  (O("LIBOR",(Day 90)))
                `cAnd` give (
                             interestSwapFxd (C(2020,7,10)) (O("Principal", 1000000))  (O("Fixed Rate", 0.01))
                            )
                `cAnd`
                             interestSwapFlt (C(2020,10,10)) (O("Principal", 1000000))  (O("LIBOR",(Day 90)))
                `cAnd` give (
                             interestSwapFxd (C(2020,10,10)) (O("Principal", 1000000))  (O("Fixed Rate", 0.01))
                            )