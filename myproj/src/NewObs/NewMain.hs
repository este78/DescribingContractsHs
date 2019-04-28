module NewMain where

import NewObs
import Simulation
import Simulation2

main :: IO ()

main =do
      simulation1
      simulation1Raw
      simulation2
      simulation2Raw  
  
 --Print format for tlogs
prettyPrint [] = "\n"  
prettyPrint tlog = let step = head tlog in ( front step ++ ", " ++ middle step ++ ", " ++ back step ++ "\n" ) ++ prettyPrint (tail tlog)

--start date (Day)
startDate = C(2019,01,01)

--Triple data extractor for printing simulation
front (a,_,_) = date2String (incrementDate startDate a)
middle (_,a,_) = show a
back (_,_,[]) = []
back (_,_, a) = rPrint (head a)


--human readable, kind of
simulation1= 
     putStrLn (
                "\nHedger Contract: " ++ rPrint wContractR ++ "\n" ++ 
               (prettyPrint (sim1  boolObs wContractR)) ++ "\n" ++ 
                "Speculator Contract: " ++ rPrint wContractP ++ "\n" ++ 
               (prettyPrint (sim1 boolObs wContractP) ++ "\n") 
              )
--show the raw data
simulation1Raw=
         putStrLn (
                   "\n" ++ show (sim1 boolObs wContractR) ++ "\n" ++ 
                          (show (sim1 boolObs wContractP) ++ "\n")
                  ) 


simulation2= 
     putStrLn (
                "\nCar Loan: "++ rPrint carLoan ++ "\n" ++ 
                (prettyPrint (sim2 doubObs carLoan)) ++ "\n" 
              )

simulation2Raw= putStrLn ("\n" ++ show (sim2 doubObs carLoan) ++ "\n" ) 
