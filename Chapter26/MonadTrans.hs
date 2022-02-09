{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
module Chapter26.MonadTrans where

import Control.Monad.Trans
import Chapter26.StateTransformer
import Chapter26.EitherTransformer

-- 1. You thought you were done with EitherT:
--
-- NOTE: A more verbose way to write this (sometimes also easier to grasp):
--   do
--    a <- ma
--    return $ Right a    
instance MonadTrans (EitherT e) where
    lift :: (Monad m) => m a -> EitherT e m a
    lift ma = EitherT $ Right <$> ma

-- 2. Or StateT. This one’ll be more obnoxious. It’s fine if you’ve seen this before:
--
-- NOTE: A more verbose way to write this (sometimes also easier to grasp):
--      do
--      a <- ma
--      return (a, s)

instance MonadTrans (StateT s) where
    lift :: (Monad m) => m a -> StateT s m a
    lift ma = StateT $ \s -> (,s) <$> ma
