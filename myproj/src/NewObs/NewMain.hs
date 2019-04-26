module NewMain where

import NewObs
import Test
import Simulation

main :: IO ()

main =do
      putStrLn (show (sim boolObs weatherContractR) ++ "\n")
	  