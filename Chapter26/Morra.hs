{-# LANGUAGE RecordWildCards #-}
module Chapter26.Morra where

import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import System.Random
import Data.List
import Data.Time.Clock
import Data.Bool (bool)
import Distribution.Simple.Utils

import Control.Monad.Trans.State

type PlayerName = String

-- each time a player bets on a number
data Bet = Bet 
    { name :: PlayerName
    , bet :: Int
    }

-- Winner of a single round
data Winner = Winner 
    { winner :: PlayerName
    , betsSum :: Int
    }

-- the Game state; It represents how's playing (names) and how many times each of those players won
data Game = Game 
    { oddsPlayer  :: PlayerName
    , evensPlayer :: PlayerName
    , oddsWins    :: Integer
    , evensWins    :: Integer    
    }

instance Show Game where
    show (Game op ep ow ew) = "\n++ " ++ op ++ " playing odds won " ++ show ow ++ ", " ++ ep ++ " playing evens won " ++ show ew ++ "\n"

-- a game starts with 0 wins for each side
mkGame :: PlayerName -> PlayerName -> Game
mkGame oPlayer ePlayer = Game oPlayer ePlayer 0 0

printInit :: Game -> IO ()
printInit (Game oPlayer ePlayer _ _) = do 
    putStrLn $ "-- " ++ oPlayer ++ " is playing odds"
    putStrLn $ "-- " ++ ePlayer ++ " is playing evens"
    putStrLn   "-- Let's get started!"

askPlayerName :: String -> IO PlayerName
askPlayerName opt = do
    putStrLn $ "How's playing " ++ opt ++ "?"
    getLine
    
-- TODO: pending validation of the bet range
readPlayerBet :: PlayerName -> IO Int
readPlayerBet player = do
    putStrLn $ "Place your bet (0-10) " ++ player ++ ":"
    fromMaybe 0 . readMaybe <$> getLine

winnerIs :: Bet -> Bet -> Winner
winnerIs oBet eBet = Winner {..}
                     where winner = if odd betsSum then name oBet else name eBet
                           betsSum = bet oBet + bet eBet
        
printResult :: Bet -> Bet -> Winner -> IO ()
printResult (Bet p1 b1) (Bet p2 b2) (Winner p s) = do
    putStrLn $ "-- Winner is " ++ p ++ " over bets sum'ing " ++ show s
    putStrLn $ "-- Playing odds: " ++ p1 ++ " played " ++ show b1
    putStrLn $ "-- Playing evens: " ++ p2 ++ " played " ++ show b2

randomBet :: IO Int 
randomBet = do 
    let roll = uniformR (0, 100)
    let rolls = unfoldr (Just . roll)
    now <- getCurrentTime 
    let nowGen = fromIntegral . diffTimeToPicoseconds . utctDayTime $ now
    let pureGen = mkStdGen nowGen
    let nextNbr = head (rolls pureGen)
    pure $ rem nextNbr 10

-- This runs a single round
singleRound :: Game -> IO Int -> IO Winner
singleRound (Game oPlayer ePlayer _ _) betEvenF = do
    oBet <- Bet oPlayer <$> readPlayerBet oPlayer
    eBet <- Bet ePlayer <$> betEvenF
    let winner = winnerIs oBet eBet
    printResult oBet eBet winner
    pure winner

-- This loops until `Ctrl+C` is hit, updating the state (win counts) of the game
gameplay :: IO Int -> StateT Game IO Winner
gameplay betEvenF = StateT $ \g@Game {..} -> do
    w <- singleRound g betEvenF
    let game = Game {..}
    let game' = game { oddsWins  = oddsWins  + bool 0 1 (winner w == oddsPlayer)
                     , evensWins = evensWins + bool 0 1 (winner w == evensPlayer) 
                     }
    print game'
    runStateT (gameplay' betEvenF) game'


initGame :: Bool -> IO Game
initGame isDual = do
    oPlayer <- askPlayerName "odds"
    ePlayer <- if isDual then askPlayerName "evens" else pure "Computer"
    let game = mkGame oPlayer ePlayer
    printInit game
    pure game

askIfPlayingAgainstAnotherPerson :: IO Bool 
askIfPlayingAgainstAnotherPerson = do
    putStrLn "Are you playing against another player. If not, I'll set the Computer as your opponent (Y/n)?"
    answer <- getLine
    let normAnswer = lowercase answer
    pure $ isPrefixOf "y" normAnswer

main :: IO ()
main = do 
    isDual <- askIfPlayingAgainstAnotherPerson
    game@(Game _ ePlayer _ _) <- initGame isDual
    let betEvenF :: IO Int 
        betEvenF = if isDual then readPlayerBet ePlayer else randomBet
    runStateT (gameplay betEvenF) game
    putStrLn "Done!"

    