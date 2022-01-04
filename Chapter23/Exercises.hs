module Chapter23.Exercises where

-- using Moi instead of State 
import Chapter23.Moi

-- Construct a State where the state is also the value you return
get :: Moi s s
get = Moi $ \s -> (,) s s 

-- Construct a State where the resulting state is the argument provided, and the value defaults to unit
put :: s -> Moi s () 
put s = Moi $ const ((), s)

-- Run the State with s and get the state that results
exec :: Moi s a -> s -> s
exec (Moi sa) = snd . sa

-- Run the State with s and get the value that results
eval :: Moi s a -> s -> a 
eval (Moi sa) = fst . sa

-- Write a function that applies a function to create a new State
modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> f <$> runMoi (put s) s