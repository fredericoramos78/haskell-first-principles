module Chapter26.ReaderTransformer where

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

-- NOTE: both `Functor` and `Applicative` have the exact same implementation
-- That's what it means to be `closed under composition`
instance Functor m => Functor (ReaderT r m) where
    fmap f (ReaderT ma) = ReaderT $ (fmap . fmap) f ma

instance Applicative m => Applicative (ReaderT r m) where
    pure a = ReaderT $ (pure . pure) a
    (ReaderT mf) <*> (ReaderT ma) = ReaderT $ mf' <*> ma
                                    where mf' = (<*>) <$> mf

instance Monad m => Monad (ReaderT r m) where
    return = pure

    (>>=) (ReaderT ma) mf = ReaderT $ \r -> do
        a <- ma r
        (runReaderT . mf) a r
        