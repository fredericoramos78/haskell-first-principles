module Chapter9.ListFunctions where 

-- Exercise 1:

-- Using takeWhile and dropWhile, write a function that takes "sheryl wants fun"
-- and turns into ["sheryl", "wants", "fun"]
myWords :: String -> [String]
myWords [] = []
myWords (' ' : xs) = myWords xs
myWords str = takeWhile isNotBlank str : myWords (dropWhile isNotBlank str)

isNotBlank :: Char -> Bool
isNotBlank x = x /= ' ' 

-- Exercise 2: Write a function that takes a string and returns a list of strings

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n" 
thirdSen = "What immortal hand or eye\n" 
fourthSen = "Could frame thy fearful\
\ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String] 
myLines "" = []
myLines str = takeWhile isNotBreakLine str : myLines (drop 1 $ dropWhile isNotBreakLine str)
    where isNotBreakLine x = x /= '\n' 

shouldEqual = [ "Tyger Tyger, burning bright" , "In the forests of the night" , "What immortal hand or eye" , "Could frame thy fearful symmetry?" ]

main :: IO () 
main =
    print $
        "Are they equal? "
        ++ show (myLines sentences == shouldEqual)

isNotChar c t = c /= t

genMyFunc :: String -> Char -> [String]
genMyFunc "" _ = []
genMyFunc str char = takeWhile (isNotChar char) str : genMyFunc (drop 1 $ dropWhile (isNotChar char) str) char


