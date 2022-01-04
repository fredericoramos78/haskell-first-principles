module Chapter23.RandomG where

import System.Random
import Control.Applicative (liftA3) 
import Control.Monad (replicateM)
import Control.Monad.Trans.State

-- Six-sided die
data Die =
      DieOne
    | DieTwo
    | DieThree
    | DieFour
    | DieFive
    | DieSix
    deriving (Eq, Show)

intToDie :: Int -> Die 
intToDie 1 = DieOne
intToDie 2 = DieTwo 
intToDie 3 = DieThree
intToDie 4 = DieFour
intToDie 5 = DieFive
intToDie 6 = DieSix
intToDie _ = error "intoToDie works only for inputs between 1 and 6"

--
-- without State 
--
rollDice :: Die 
rollDice = intToDie d 
            where (d, _) = randomR (1, 6) (mkStdGen 0) :: (Int, StdGen)

roll3Dices :: (Die, Die, Die)
roll3Dices = (intToDie d1, intToDie d2, intToDie d3) 
                where (d1, g1) = randomR (1, 6) (mkStdGen 5) :: (Int, StdGen)
                      (d2, g2) = randomR (1, 6) g1 :: (Int, StdGen)
                      (d3, g3) = randomR (1, 6) g2 :: (Int, StdGen)

--
-- with State
--
rollDice' :: State StdGen Die
rollDice' = state $ do 
              (n, g) <- randomR (1, 6) :: StdGen -> (Int, StdGen)
              return (intToDie n, g)

-- This will keep producing the same number since rollDice' is a `value`  (:: State StdGen Die) and not a function being 
--    applied multiple times
infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDice'

infiniteDie' :: Int -> State StdGen [Die]
infiniteDie' n = replicateM n rollDice' 


-- rolling up to 20
rollsToGetTwenty :: Int -> StdGen -> (Int, [Int])
rollsToGetTwenty limit = go limit 0 0 []
                      where go :: Int -> Int -> Int -> [Int] -> StdGen -> (Int, [Int])
                            go limit sum count dices gen
                              | sum >= limit = (count, dices)
                              | otherwise = go limit (sum + rolledDice) (count + 1) (rolledDice : dices) nextGen
                                              where (rolledDice, nextGen) = randomR (1, 6) gen   
                                