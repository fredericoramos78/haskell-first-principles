{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module Chapter11.LogicGoats where 


class TooMany a where 
    tooMany :: a -> Bool

instance TooMany Int where 
    tooMany n = n > 42


-- 1. Reusing the TooMany type class, write an instance of the type class for the type (Int, String). 
-- This will require adding a language pragma named FlexibleInstances if you do not use a newtype — GHC will 
-- tell you what to do.

instance TooMany (Int, String) where 
    tooMany (n, _) = n > 42

-- 2. Make another TooMany instance for (Int, Int). Sum the values together under the assumption that this is 
-- a count of goats from two fields.

instance TooMany (Int, Int) where 
    tooMany (n, m) = (n + m) > 42

-- 3. Make another TooMany instance, this time for (Num a, TooMany a) => (a, a). This can mean whatever you want, 
-- such as summing the two numbers together.

-- ???