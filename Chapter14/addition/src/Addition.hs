module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a) 
dividedBy num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)

multiplyBy :: (Eq a, Num a) => a -> a -> a
multiplyBy _ 0 = 0
multiplyBy a n
    | n == 0 = 0
    | n == 1 = a 
    | otherwise = a + multiplyBy a (n-1)

main :: IO ()
main = hspec $ do
    describe "Adition" $ do
        it "1+1 is greater than 1" $ do 
            ((1 + 1) :: Integer) > (1 :: Integer) `shouldBe` True
        it "2+2 is equal to 4" $ do
            ((2+2) :: Integer) `shouldBe` (4 :: Integer)
        it "x+1 is always greater than 1" $ do 
            property $ \x -> x + 1 > (x :: Int)
    describe "dividedBy" $ do
        it "15 divided by 3 is 5" $ do
            dividedBy (15 :: Integer) (3 :: Integer) `shouldBe` (5, 0)
        it "22 divided by 5 is 4 with remainder 2" $ do
            dividedBy (22 :: Integer) (5 :: Integer) `shouldBe` (4, 2)
    describe "multiplyBy" $ do
        it "any number multiplied by 0 is 0" $ do
            multiplyBy  (2 :: Integer) (0 :: Integer) `shouldBe` 0
            multiplyBy (20 :: Integer) (0 :: Integer) `shouldBe` 0
        it "5 multiplied by 1 is 5" $ do
            multiplyBy  (5 :: Integer) (1 :: Integer) `shouldBe` 5
        it "3 multiplied by 3 is 9" $ do
            multiplyBy  (3 :: Integer) (3 :: Integer) `shouldBe` 9
        it "multiplication is associative" $ do
            multiplyBy  (5 :: Integer) (1 :: Integer) `shouldBe`  multiplyBy  (1 :: Integer) (5 :: Integer)

