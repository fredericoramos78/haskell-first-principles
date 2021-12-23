module Chapter18.MonadExamples where

twiceWhenEven :: [Integer] -> [Integer] 
twiceWhenEven xs = do
    x <- xs 
    if even x
        then [x*x, x*x] 
        else [x*x]

twiceWhenEven' :: [Integer] -> [Integer] 
twiceWhenEven' xs = do
    x <- xs 
    if even x
        then [x*x, x*x] 
        else []

twiceWhenEvenM :: [Integer] -> [Integer] 
twiceWhenEvenM xs = xs >>= (\x -> if even x then [x*x, x*x] else [x*x])

twiceWhenEvenM' :: [Integer] -> [Integer] 
twiceWhenEvenM' xs = xs >>= (\x -> if even x then [x*x, x*x] else [])
