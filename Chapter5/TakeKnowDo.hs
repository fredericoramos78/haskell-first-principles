module Chapter5.TakeKnowDo where 


-- 1.
f :: Int -> String 
f = undefined

g :: String -> Char 
g = undefined

h :: Int -> Char
h a = g $ f a

-- 2. 
data A 
data B 
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e a = w $ q a

-- 3. 
data X 
data Y 
data Z
xz :: X -> Z 
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (a, b) = (xz a, yz b) 

-- 4. 
munge :: (x -> y) -> (y -> (w, z)) -> x -> w 
munge f1 f2 a = fst $ f2 $ f1 a 