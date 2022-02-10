{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
module Chapter26.MonadIO where

import Control.Monad.IO.Class
import Chapter26.EitherTransformer
import Chapter26.MaybeTransformer
import Chapter26.ReaderTransformer
import Chapter26.StateTransformer
import Chapter26.MonadTrans


-- 1. IdentityT
newtype IdentityT f a = IdentityT { runIdentityT :: f a } deriving (Eq, Show)

instance (Functor m) => Functor (IdentityT m) where
    fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance (Applicative m) => Applicative (IdentityT m) where
    pure a = IdentityT $ pure a
    (<*>) (IdentityT ff) (IdentityT fa) = IdentityT $ ff <*> fa

instance (Monad m) => Monad (IdentityT m) where
    return = pure
    (>>=) (IdentityT ma) mf = IdentityT $ ma >>= (runIdentityT . mf) 


instance (MonadIO m) => MonadIO (IdentityT m) where
    liftIO = IdentityT . liftIO

instance (MonadIO m) => MonadIO (EitherT e m) where
    liftIO ma = EitherT $ Right <$> liftIO ma

-- 1. MaybeT
instance (MonadIO m) => MonadIO (MaybeT m) where
    liftIO :: (Monad m) => IO a -> MaybeT m a
    liftIO ma = MaybeT $ Just <$> liftIO ma

-- 2. ReaderT
instance (MonadIO m) => MonadIO (ReaderT r m) where
    liftIO :: (Monad m) => IO a -> ReaderT r m a
    liftIO ma = ReaderT $ \r -> liftIO ma

-- 3. StateT
instance (MonadIO m) => MonadIO (StateT s m) where
    liftIO :: (Monad m) => IO a -> StateT s m a
    liftIO ma = StateT $ \s -> (, s) <$> liftIO ma