module Chapter11.FunctionCardinality where

-- Consider the following function:
-- convert :: Quantum -> Bool 
-- convert = undefined
-- According to the equality of a -> b and ba, there should be 2^3 or 8 implementations of this function. 
-- Does this hold? Write it out, and prove it for yourself.


data Quantum = Yes
             | No
             | Both
             deriving (Eq, Show)

convert :: Quantum -> Bool 
convert Yes = True
convert Yes = False 
convert No = True
convert No = False 
convert Both = True
convert Both = False 

