module Pprint (pprint) where


import Contract

---- naive pprinter to text representation of contract
pprint :: Contract -> String
--pprint a = ""
pprint Zero = "Zero"
pprint (One k) = "One(" ++ show k ++ ")"
pprint (Give c) = "Give(" ++ pprint c ++ ")"
pprint (And c1 c2) = "And(" ++ pprint c1 ++ "," ++ pprint c2 ++ ")"
pprint (Or c1 c2) = "Or(" ++ pprint c1 ++ "," ++ pprint c2 ++ ")"
pprint (Scale obs c) = "ScaleObs(" ++ pprint c ++ ")"
pprint (Anytime obs c) = "Anytime(" ++ pprint c ++ ")"

  
--pprint (Cond obs c) =  
--pprint (When obs c) = "When at(" ++
--print  (Until obs c) = "Until"