module Chapter12.EitherLib where

-- 1. Try to eventually arrive at a solution that uses foldr, even if earlier versions don’t use foldr:

leftOrEmpty :: Either a b -> [a]
leftOrEmpty (Left a) = [a]
leftOrEmpty (Right _) = []

lefts' :: [Either a b] -> [a]
lefts' = foldr (\e a -> leftOrEmpty e ++ a) []  

-- 2. Same as the last one. Use foldr, eventually:

rightOrEmpty (Right a) = [a]
rightOrEmpty (Left _) = []

rights' :: [Either a b] -> [b]
rights' = foldr (\e a -> rightOrEmpty e ++ a) []   


either2Tuple :: Either a b -> ([a], [b])
either2Tuple (Left a) = ([a], [])
either2Tuple (Right b) = ([], [b])

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldl (\(a, b) e -> let (a', b') = either2Tuple e in (a ++ a', b ++ b')) ([], [])

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _)  = Nothing 
eitherMaybe' f (Right a) = Just . f $ a

-- This is a general catamorphism for Either values:
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' funcL _ (Left a)  = funcL a
either' _ funcR (Right b) = funcR b

-- Same as before, but use the either' function you just wrote:
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' _ (Left _) = Nothing 
eitherMaybe'' f r@(Right b)= Just (either' (\_ -> f b) f r)
