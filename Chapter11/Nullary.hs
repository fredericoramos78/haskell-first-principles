module Chapter11.Nullary where

-- 1. You can query the type of a value in GHCi with the :type command, also abbreviated :t.
--    Example:
--    Prelude> :t False
--    False :: Bool
-- What is the type of the data constructor MakeExample? What happens when you request the type of Example?
-- Solution:
-- a) MakeExample :: Example
-- b) Fails b/c Example is not a data which has a type; it's the type itself 

-- 2. What if you try :info on Example in GHCi? Can you determine what type class instances are defined for 
-- the Example type using :info in GHCi?
-- Solution: Now it works and it has Show as a single type class instance 

-- 3. Try making a new datatype like Example but with a single type argument added to MakeExample, such as Int. 
-- What has changed when you query MakeExample with :type in GHCi?
-- Solution: The data constructor is show as if it was a function, since now it requires an Int to be applied 
-- before materializing into a proper value.
-- MakeExample :: Int -> Example