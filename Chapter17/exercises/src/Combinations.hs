module Combinations where

import Control.Applicative (liftA3) 

-- Given the following sets of consonants and vowels:
stops  = "pbtdkg"
vowels = "aeiou"

-- this is from Chapter 10
svs = [ (x, y, z) | x <- stops, y <- vowels, z <- stops ]

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

svsEqCombos = combos stops vowels stops == svs 
