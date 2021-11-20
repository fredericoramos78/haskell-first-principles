module Chapter12.MaybeLib where

-- Write the following functions. This may take some time. 

-- 1. Simple Boolean checks for Maybe values:
isJust :: Maybe a -> Bool 
isJust (Just _) = True 
isJust _ = False 

isNothing :: Maybe a -> Bool
isNothing = not . isJust 

-- 2. The following is the Maybe catamorphism. You can turn a Maybe value into anything else with this:
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee d f Nothing = d
mayybee _ f (Just a) = f a

-- 3. In case you just want to provide a fallback value. Try writing it in terms of the maybe catamorphism:
fromMaybe :: a -> Maybe a -> a
fromMaybe a = mayybee a id

-- 4. Converting between List and Maybe:
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing 
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- 5. For when we want to drop the Nothing values from a list: 
catMaybes :: [Maybe a] -> [a]
catMaybes = concatMap maybeToList  

-- 6. You’ll see this called sequence later:
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe ms = let cs = catMaybes ms
               in case cs of 
                   []  -> Nothing 
                   xs  -> Just xs 




