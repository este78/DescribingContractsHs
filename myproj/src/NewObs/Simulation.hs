module Simulation where

import NewObs
import Test


 --11. a notion of the current time
 -- 12. a way to enter/make up future INDEPENDENT observable values
 -- 13. a way to step time forward
 -- 14. some whay to record history - transaction log
 -- 15. Given current time, a way to see if contract has been triggered.
     -- simulate today futureObs contract
      -- | triggered contract today = .....
      -- | otherwise = simulate tomorrow futureObs' contract
 -- 16. When choices are required, how are these input.
    -- 16a - as part of future indepenent observables
    -- 16b - as the user?

-- simulate today futureObs contract
       -- | triggered contract today = .....
       -- | otherwise = simulate tomorrow futureObs' contract
-- --

-- today [Date] -> Date
-- today x:xs = C x


today = take 1096 (iterate mkDate time0)


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
        | otherwise  = O("LIBOR 3month", 0.0)
--
priceToday s t | s == "Oil(Brent)" && t == C(2020,4,26) = O(s ++ date2String t,0.47)
               | s == "Oil(Brent)" && t == C(2020,4,27) = O(s ++ date2String t,0.48)
               | s == "Oil(Brent)" && t == C(2020,4,28) = O(s ++ date2String t,0.48)
               | s == "Oil(Brent)" && t == C(2020,4,29) = O(s ++ date2String t,0.49)
               | s == "Gold gr." && t == C(2020,4,26) = O(s ++ date2String t,41.03)
               | s == "Gold gr." && t == C(2020,4,27) = O(s ++ date2String t,41.23)
               | s == "Gold gr." && t == C(2020,4,28) = O(s ++ date2String t,40.95)
               | s == "Gold gr." && t == C(2020,4,29) = O(s ++ date2String t,41.5)
               |otherwise = O(s ++ date2String t , 0.0)