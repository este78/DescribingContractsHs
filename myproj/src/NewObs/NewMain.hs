module NewMain where

import NewObs

import Simulation

main :: IO ()

main =do
      simulation1
      simulation1Raw
     
  
  
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
simulation1= putStrLn ( "\nHedger Contract: \n" ++ (prettyPrint (sim1  boolObs wContractR)) ++ "\n" ++ "Speculator Contract: \n" ++ (prettyPrint (sim1 boolObs wContractP) ++ "\n") )
--show the raw data
simulation1Raw= putStrLn ("\n" ++ show (sim1 boolObs wContractR) ++ "\n" ++ (show (sim1 boolObs wContractP) ++ "\n")) 