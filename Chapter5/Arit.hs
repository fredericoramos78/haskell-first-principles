-- err1: module names are capitalized
module Chapter5.Arith3Broken where

main :: IO () 
main = do
    -- needed $ between print and the addition
    print $ 1 + 2
    -- needed to transform 10::Num into 10::String (=`show`)
    putStrLn $ show 10
    -- needed parenthesis to have -1 been treated as a negative 
    --      number and not the subtract op
    print (negate (-1))
    print ((+) 0 blah) 
    -- identation was wrong
        where blah = negate 1