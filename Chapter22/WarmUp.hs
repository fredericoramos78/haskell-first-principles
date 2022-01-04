module Chapter22.WarmUp where

import Data.Char

cap :: [Char] -> [Char] 
cap = map toUpper

rev :: [Char] -> [Char] 
rev = reverse

composed = cap . rev 

fmapped = fmap cap rev 


tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev 

tupledM :: [Char] -> ([Char], [Char])
tupledM = do 
    c <- cap 
    r <- rev 
    return (c, r)

tupledM' :: [Char] -> ([Char], [Char])
tupledM' = cap >>= (\x r -> (x, rev r)) 
           
-- (>>=) :: Monad m       => m a      -> (a -> m b)    -> m b
--            (_ -> a)    => (a -> a) -> (a -> a -> b) -> (a -> b)

--          cap          >>=     (,)
--          m = (->) r          
--          a = [Char]
--                            ([Char] -> r -> b)   (\x r -> (x, rev r))
--                                    -> [Char]
