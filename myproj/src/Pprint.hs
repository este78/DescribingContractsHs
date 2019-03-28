
module Pprint where


import Contract



---- naive pprinter to text representation of contract
pprint :: Contract -> String
--pprint (Pure a) = ""
pprint Zero = "Zero"
pprint (One k) = "One(" ++ show k ++ ")"
pprint (Give c) = "Give(" ++ pprint c ++ ")"
pprint (And c1 c2) = "And(" ++ pprint c1 ++ "," ++ pprint c2 ++ ")"
pprint (Or c1 c2) = "Or(" ++ pprint c1 ++ "," ++ pprint c2 ++ ")"
pprint (Truncate t c) = "Truncate(" ++ show t ++ "," ++ pprint c ++ ")"
pprint (Scale obs c) = "ScaleObs(" ++ pprint c ++ ")"
pprint (Get c) = "Get(" ++ pprint c ++ ")"
pprint (Anytime c) = "Anytime(" ++ pprint c ++ ")"

