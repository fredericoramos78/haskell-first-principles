module Chapter11.Quad where


data Quad = One
          | Two
          | Three
          | Four
          deriving (Eq, Show)
     
-- 1. How many different forms can this take?
eQuad :: Either Quad Quad
eQuad = undefined
-- eQuad = product (4, 4)
-- eQuad = 4 * 4 = 16

prodQuad :: (Quad, Quad)
prodQuad = undefined
-- prodQuad = (4, 4)
-- prodQuad = 4 * 4 = 16

funcQuad :: Quad -> Quad
funcQuad = undefined
-- funcQuad = 4 ^ 4 = 256

prodTBool :: (Bool, Bool, Bool)
prodTBool = undefined
-- prodTBool = (2, 2, 2)
-- prodTBool = 2 * 2 * 2 = 8

gTwo :: Bool -> Bool -> Bool
gTwo = undefined
-- gTwo = 2 ^ 2 ^ 2
-- gTwo = 2 ^ 4 = 16

fTwo :: Bool -> Quad -> Quad
fTwo = undefined 
-- fTwo = 2 ^ 4 ^ 4
-- fTwo = 2 ^ 16 = 65536