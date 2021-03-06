module ReplaceExperiment where 

replaceWithP :: b -> Char 
replaceWithP _ = 'p' 

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

-- making the argument a bit more specific
replaceWithP' :: [Maybe [Char]] -> Char 
replaceWithP' = replaceWithP 

liftedReplace :: Functor f => f a -> f Char 
liftedReplace = fmap replaceWithP 

-- a bit more specific; replacing Functor f with []
liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace 


-- now lifting twice
twiceLifted :: (Functor f1, Functor f2) => f1 (f2 a) -> f1 (f2 Char)
twiceLifted = (fmap . fmap) replaceWithP 

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

-- three times now
thriceLifted :: (Functor f1, Functor f2, Functor f3) => f1 (f2 (f3 a)) -> f1 (f2 (f3 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP 

thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

main :: IO () 
main = do
  putStr "replaceWithP' lms:   "
  print (replaceWithP' lms)
  putStr "liftedReplace lms:   "
  print (liftedReplace lms)
  putStr "liftedReplace' lms: "
  print (liftedReplace' lms)
  putStr "twiceLifted lms:     "
  print (twiceLifted lms)
  putStr "twiceLifted' lms:    "
  print (twiceLifted' lms)
  putStr "thriceLifted lms:    "
  print (thriceLifted lms)
  putStr "thriceLifted' lms:   "
  print (thriceLifted' lms)