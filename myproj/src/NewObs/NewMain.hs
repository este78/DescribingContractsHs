module NewMain where

import NewObs
import Test
import Simulation

main :: IO ()

main =do
      simulation1
      
     
  
  
 --Print format for tlogs
prettyPrint [] = "\n"  
prettyPrint tlog = let step = head tlog in ( front step ++ ", " ++ middle step ++ ", " ++ back step ++ "\n" ) ++ prettyPrint (tail tlog)

--Triple data extractor
front (a,_,_) = date2String a
middle (_,a,_) = show a
back (_,_,[]) = []
back (_,_, a) = rPrint (head a)


--human readable, kind of
simulation1= putStrLn ( "\nHedger Contract: \n" ++ (prettyPrint (sim1 boolObs weatherContractR)) ++ "\n" ++ "Speculator Contract: \n" ++ (prettyPrint (sim1 boolObs weatherContractP) ++ "\n") )
--show the raw data
simulation1Raw= putStrLn (show (sim1 boolObs weatherContractR) ++ "\n" ++ (show (sim1 boolObs weatherContractP) ++ "\n")) 