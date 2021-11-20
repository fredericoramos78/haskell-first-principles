module Chapter3.Exercises where 

-- Given "Hello World" returns "ello World"
exercise1 str = drop 1 str
exercise1' = exercise1 "Hello world" 

-- Given "Curry is awesome" returns "Curry is awesome!"
exercise2 str = str ++ "!"
exercise2' = exercise2 "Curry is awesome"

-- Given "Curry is awesome!" returns "y"
exercise3 str = str !! 4
exercise3' = exercise3 "Curry is awesome"

-- Given "Curry is awesome!" returns "awesome!"
exercise4 str = drop 9 str
exercise4' = exercise4 "Curry is awesome!"

thirdChar :: [Char] -> Char 
thirdChar str = str !! 2

nthChar :: Int -> Char 
nthChar pos = "Curry is awesome!" !! (pos-1)

-- reversing the string "Curry is awesome"
rvrs :: [Char]
rvrs = first ++ second ++ third
    where first = drop 9 sentence
          second = take 4 $ drop 5 sentence
          third = take 5 sentence
          sentence = "Curry is awesome"


