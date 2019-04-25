module NewMain where

import NewObs
import Test
import Numeric

main :: IO ()

main =do
       putStr ("Filename: ")
       fname <- getLine
       txt <- readFile fname
       putStr "continue?"
       getLine
       let creads = read txt
       putStrLn ("Have "++show (length creads)++"(s) contracts")
       putStr ("Pick=one 0,1,2,...length-1 \n")
       txt <- getLine
       let c = creads!!(read txt)
       putStrLn ("Read:\n"++ rPrint c)
       main

showcontracts [] = putStrLn "\nDone!"
showcontracts (c:cs)
 = do putStrLn ("Read:\n"++ rPrint c)
      putStr "continue?"
      getLine
      showcontracts cs

