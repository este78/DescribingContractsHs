module Simulation where

import NewObs
import Test
import System.Environment

 


 -- 14. some way to record history - transaction log
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


-- 11. a notion of the current time  time0= 2019/04/29 ; 3 year calendar from 29/04/2019
calendar = take 1097 (iterate mkDate time0)

--To travel through the Calendar List
iterator = take 1097 (iterate count 0)
count x = x  + 1

--Retrieves a day out of the calendar
today  n = calendar !! n



-- 14. some way to record history - transaction log
--transactionLog :: String -> String -> String
transactionLog str [] = str  
transactionLog str2 str1 = str2 ++ str1



                   
-- 12. a way to enter/make up future INDEPENDENT observable values
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