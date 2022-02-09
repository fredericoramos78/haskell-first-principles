{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Chapter26.Exercises where

import Control.Monad.Trans.Reader (ReaderT, runReaderT, ReaderT(ReaderT))
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Monad.Identity
import Chapter26.MonadIO

newtype Reader r a = Reader { runReader :: r -> a }

-- 1. `rDec` is a function that should get its argument in the context of Reader and return a value decremented by one:
rDec :: Num a => Reader a a 
rDec = Reader f 
        where f a = a - 1

--     Prelude> import Control.Monad.Trans.Reader
--     Prelude> runReader rDec 1
--     0
--     Prelude> fmap (runReader rDec) [1..10]
--     [0,1,2,3,4,5,6,7,8,9]
-- Note that Reader from transformers is the ReaderT of Identity and that runReader is a convenience function that throws 
--   away the meaningless structure for you. Play with runReaderT if you like.

-- 2. Once you have a n `rDec` that works, make it and any inner lambdas point-free, if that’s not already the case.

-- 3. rShow is show, but in Reader:

rShow :: (Show a) => ReaderT a Identity String
rShow = ReaderT $ \r -> do 
        return $ show r
    
--     Prelude> runReaderT rShow 1
--     "1"
--     Prelude> fmap (runReaderT rShow) [1..10]
--     ["1","2","3","4","5","6","7","8","9","10"]

-- 4. Once you have an `rShow` that works, make it point-free.

-- 5. `rPrintAndInc` will first print the input with a greeting, then return the input incremented by one:
rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> do
        putStrLn $ "Hi: " ++ show r
        return $ r + 1

--   Prelude> runReaderT rPrintAndInc 1
--     Hi: 1
--     2
--     Prelude> traverse (runReaderT rPrintAndInc) [1..10]
--     Hi: 1
--     Hi: 2
--     Hi: 3
--     Hi: 4
--     Hi: 5
--     Hi: 6
--     Hi: 7
--     Hi: 8
--     Hi: 9
--     Hi: 10
--     [2,3,4,5,6,7,8,9,10,11]

-- 6. sPrintIncAccum first prints the input with a greeting, then “puts” the incremented input as the new state and 
-- returns the original input as a String:
sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
        putStrLn $ "Hi: " ++ show s
        return (show s, s+1)
--     Prelude> runStateT sPrintIncAccum 10
--     Hi: 10
--     ("10",11)
--     Prelude> mapM (runStateT sPrintIncAccum) [1..5]
--     Hi: 1
--     Hi: 2
--     Hi: 3
--     Hi: 4
--     Hi: 5
--     [("1",2),("2",3),("3",4),("4",5),("5",6)]