module Chapter5.TypeInference where 

f :: Num a => a -> a -> a 
f a b = a + b + 3
