{-# LANGUAGE InstanceSigs #-}
module Chapter26.NoCommute where 

import Chapter26.MaybeTransformer
import Chapter26.ReaderTransformer

newtype Reader r a = Reader { runReader :: r -> a }

-- Both are the same!


x :: ReaderT r Maybe Int
x = ReaderT $ \r -> Just 10
-- ReaderT r m a     = ReaderT { runReaderT :: r -> m a }
-- ReaderT r Maybe a = ReaderT { runReaderT :: r -> Maybe a }
-- ------- `r -> Maybe a` is the same as `Reader r (Maybe a)`

x' :: MaybeT (Reader r) Int
x' = MaybeT $ Reader $ \r -> Just 10
-- MaybeT m a          = MaybeT { runMaybeT :: m (Maybe a) }
-- MaybeT (Reader r) a = MaybeT { runMaybeT :: Reader r (Maybe a) } 
-- ------- `Reader r (Maybe a)` is the same as `Reader { runReader :: r -> Maybe a }`
--         which can also be `ReaderT r Maybe a`

f :: Bool
f = x1 () == x1' ()
    where x1  = runReaderT x
          x1' = (runReader . runMaybeT) x'
