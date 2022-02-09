{-# LANGUAGE RecordWildCards #-}
module Chapter26.Morra where

import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import System.Random
import Data.List
import Data.Time.Clock
import Data.Bool (bool)
import Distribution.Simple.Utils

type PlayerName = String

data Bet = Bet 
    { name :: PlayerName
    , bet :: Int
    }

data Winner = Winner 
    { winner :: PlayerName
    , betsSum :: Int
    }
data Game = Game 
    { oddsPlayer  :: PlayerName
    , evensPlayer :: PlayerName
    , oddsWins    :: Integer
    , evensWins    :: Integer    
    }

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

main2Players :: IO ()
main2Players = do
    oPlayer <- askPlayerName "odds"
    ePlayer <- askPlayerName "evens"
    let game = mkGame oPlayer ePlayer
    printInit game
    oBet <- Bet oPlayer <$> readPlayerBet oPlayer
    eBet <- Bet ePlayer <$> readPlayerBet ePlayer
    let winner = winnerIs oBet eBet
    printResult oBet eBet winner

mainSinglePlayer :: IO ()
mainSinglePlayer = do
    oPlayer <- askPlayerName "odds"
    let ePlayer = "Computer"
    let game = mkGame oPlayer ePlayer
    printInit game
    oBet <- Bet oPlayer <$> readPlayerBet oPlayer
    eBet <- Bet ePlayer <$> randomBet
    let winner = winnerIs oBet eBet
    printResult oBet eBet winner

isPlayingDualPlayers :: IO Bool 
isPlayingDualPlayers = do
    putStrLn "Are you playing against another player. If not, I'll set the Computer as your opponent (Y/n)?"
    answer <- getLine
    let normAnswer = lowercase answer
    return $ isPrefixOf "y" normAnswer

main :: IO ()
main = do 
    isDual <- isPlayingDualPlayers
    bool mainSinglePlayer main2Players isDual