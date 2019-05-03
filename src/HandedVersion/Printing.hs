module Printing where


import NewObs
import CombDate
import Combinators

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Printing
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
--Contract Indentantion control
indent :: Int -> String -> String
indent i str  = "\n" ++ (replicate i ' ') ++ str

--Prints RECEIVABLE contracts
rPrint :: Contract -> String
rPrint c = case c of
    Zero 
           -> indent 1 "Contract with no obligations, no rights."

    One k 
           ->  " " ++ show k 

    Give u 
           -> indent 2 "GIVE " ++ pPrint u

    And u1 u2
           -> rPrint u1 ++ indent 2 "AND" ++ indent 1 ( rPrint u2 )

    Or u1 u2 
           -> indent 2 "OPTION " ++ rPrint u1 ++ indent 1 
              "OR" ++ indent 2 "OPTION " ++ rPrint u2
   
    Cond (IsTrue (O(o1,o2))) u1 u2 
           -> indent 1 "IF "++ o1 ++ " is " ++ show o2 ++ indent 2 (rPrint u1)
              ++ indent 1 "OTHERWISE" ++ indent 2(rPrint u2)
    
    Scale (O(o1,o2)) u 
           -> indent 1 "RECEIVE " ++ o1 ++ rPrint u ++ " " ++ show o2 ++ " "
    
    When (At(O(o1,o2))) u1
           -> indent 0 "On the "++ date2String (incrementDate time0 o2)
              ++ rPrint u1 ++ "\n"  
    
    Anytime (Between(O(o1,o2)) (O(o3,o4))) u 
           -> indent 0 "Contract executable between " ++ date2String o2 ++ 
               " and " ++ date2String o4 ++ indent 2 (rPrint u)

    Until (At(O(o1,o2))) u 
           -> indent 0 "Until " ++ o1 ++ " " ++ rPrint u ++ "\n"
  

--Same as print but for PAYABLE contracts
pPrint :: Contract -> String
pPrint u = case u of 
    Zero 
          -> indent 1 "Contract with no obligations, no rights."
    
    One k 
          ->  " " ++ show k 

    Give u 
          -> indent 2 "GIVE " ++ rPrint u  

    And u1 u2
          -> pPrint u1 ++ indent 2 "AND" ++ indent 1 ( pPrint u2 )

    Or u1 u2 
          -> indent 2 "OPTION " ++ pPrint u1 ++ indent 1 
             "OR" ++ indent 2 "OPTION " ++ pPrint u2

    Cond (IsTrue (O(o1,o2))) u1 u2 
          -> indent 1 "IF "++ o1 ++ " is " ++ show o2 ++ 
             indent 2 (pPrint u1) ++ indent 2 "OTHERWISE" ++ 
             indent 2 (pPrint u2)

    Scale (O(o1,o2)) u 
          -> indent 1 o1 ++ pPrint u ++ " " ++ show o2 ++ " "

    When (At(O(o1,o2))) u1 
          -> indent 0 "On the " ++ date2String (incrementDate time0 o2) 
             ++ pPrint u1 ++ "\n" 
  
    Anytime (Between(O(o1,o2)) (O(o3,o4))) u 
          -> indent 0 "Contract executable between " ++ date2String o2 ++
             " and " ++ date2String o4  ++ indent 2 (pPrint u)

    Until (At (O(o1,o2))) u 
          -> indent 0 "Until " ++ o1  ++ " " ++ pPrint u ++ "\n"

-- 
