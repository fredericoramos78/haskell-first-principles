module Chapter5.Parametricity where 


-- exercise 2: only two possible implementations of a -> a -> a
f :: a -> a -> a
f a b = a
--f a b = b 

-- exercise 3: how many implementations of a -> b -> b?
-- answer: 1
g :: a -> b -> b 
g a b = b 