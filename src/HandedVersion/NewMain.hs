module NewMain where

import NewObs
import CombDate

import Combinators
import Printing

import Simulation
import Simulation2
import Simulation3
import Test

main :: IO ()

main =do
        pause
        contractHSim1
        pause
        contractSSim1
        pause
        simulation1
        pause
        simulation1Raw
        pause
        contractSim2
        pause
        simulation2
        pause
        simulation2Raw
        pause
        contractSim2'
        pause
        simulation2'
        pause
        simulation2Raw' 
        pause
        contractSim3
        pause
        simulation3
        putStrLn "Finished, Thank You. \n"

pause = do putStr "Continue?"
           getChar 

--Print format for tlogs
prettyPrint [] = "\n"  
prettyPrint tlog = 
     let step = head tlog 
       in ( front step ++ ", " ++ middle step ++ ", " ++ back step ++ "\n" )
          ++ prettyPrint (tail tlog)

--start date (Day)
startDate = C(2020,01,01)

--Triple data extractor for printing simulation
front (a,_,_) = date2String (incrementDate startDate a)
middle (_,a,_) = show a
back (_,_,[]) = []
back (_,_, a) = rPrint (head a)


--human readable contracts

-- Weather Contract =========================================================
contractHSim1 = 
     putStrLn("\nHedger Contract: "++ rPrint wContractR ++ "\n")

contractSSim1 =
     putStrLn("Speculator Contract: "++ rPrint wContractP ++ "\n" )  

simulation1 = 
     putStrLn (
                "\nHedger Contract: "  ++ 
               (prettyPrint (sim1  boolObs wContractR)) ++ "\n" ++ 
                "Speculator Contract: " ++ 
               (prettyPrint (sim1 boolObs wContractP)) ++ "\n" 
              )

--show the raw data
simulation1Raw =
         putStrLn (
                   "\n" ++ show (sim1 boolObs wContractR) ++ "\n" ++ 
                          (show (sim1 boolObs wContractP) ++ "\n")
                  ) 

-- Loan Contract ============================================================
contractSim2 =
     putStrLn ("\n Microsft: "++ rPrint microsoft ++ "\n")
 
simulation2 = 
     putStrLn (
                (prettyPrint (sim2 doubObs microsoft)) ++ "\n" 
              )

simulation2Raw= putStrLn ("\n" ++ show (sim2 doubObs microsoft) ++ "\n" )

contractSim2' =
     putStrLn ("\n Intel: "++ rPrint intel ++ "\n")
 
simulation2' = 
     putStrLn (
                (prettyPrint (sim2 doubObs intel)) ++ "\n" 
              )

simulation2Raw'= putStrLn ("\n" ++ show (sim2 doubObs intel) ++ "\n" ) 

-- Option Contract ==========================================================

contractSim3 = 
     putStrLn ("\nOption Contract: " ++ rPrint eurOption ++ "\n")

simulation3 = 
     putStrLn (
                prettyPrint (sim3 bObs eurOption) ++ "\n"
              )

