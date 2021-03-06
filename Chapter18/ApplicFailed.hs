module Chapter18.ApplicFailed where

f :: Integer -> Maybe Integer 
f 0 = Nothing
f n = Just n

g :: Integer -> Maybe Integer 
g i =if even i
    then Just (i + 1) 
    else Nothing

h :: Integer -> Maybe String
h i = Just ("10191" ++ show i)


doSomething n = do 
    a <- f n
    b <- g a
    c <- h b
    pure (a, b, c)

-- can't do this as x n will raise a `Maybe (Maybe n)`
-- h1 n = h <$> x n
--     where x n = g <$> f n
-- doSomething' n = 
--    Just h1 <*> Just n