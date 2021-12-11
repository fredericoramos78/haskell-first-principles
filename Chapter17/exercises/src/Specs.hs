module Specs where

-- 1. 
-- Type: []
-- Methods
-- pure :: a -> A a
-- (<*>) :: A (a -> b) -> A a -> A b

-- 2. 
-- Type: IO
-- Methods
-- pure :: a -> IO a
-- (<*>) :: IO (a -> b) -> IO a -> IO b

-- 3. 
-- Type: (,) a
-- Methods
-- pure :: a -> (,) a
-- (<*>) :: (,) (a -> b) -> (,) a -> (,) b

-- 4. 
-- Type (->) e
-- Methods
-- pure ::a-> (->) a
-- (<*>) :: (->) (a -> b) -> (->) a -> (->) b

