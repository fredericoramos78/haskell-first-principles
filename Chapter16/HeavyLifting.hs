module Chapter16.HeavyLifting where

-- a = (+1) $ read "[1]" :: [Int]
a = fmap (+1) $ read "[1]" :: [Int]

-- b = (++ "lol") (Just ["Hi,", "Hello"])
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- c = (*2) (\x -> x - 2)
c = fmap (*2) (\x -> x - 2)

-- d = ((return '1' ++) . show) (\x -> [x, 1..3])
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

-- e :: IO Integer
-- e = let ioi = readIO "1" :: IO Integer
--         changed = read ("123"++) show ioi 
--     in (*3) changed
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        -- show will turn Integer -> String, then concat with "123", finally read "123x" String back into Integer
        changed = fmap (read . ("123" ++) . show) ioi
     in fmap (*3) changed