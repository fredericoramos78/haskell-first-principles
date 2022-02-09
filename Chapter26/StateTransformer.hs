{-# LANGUAGE TupleSections #-}
module Chapter26.StateTransformer where

newtype StateT s m a =
    StateT { runStateT :: s -> m (a,s) }

-- NOTE: both `Functor` and `Applicative` have the exact same implementation
-- That's what it means to be `closed under composition`
instance Functor m => Functor (StateT s m) where
    fmap f (StateT ma) = StateT $ \s -> (,s) . f . fst <$> ma s

instance Applicative m => Applicative (StateT s m) where
    pure a = StateT $ \s -> pure (a,s)
    (StateT mf) <*> (StateT ma) = StateT $ \s -> (,s) <$> (mf' s <*> ma' s)
                                    where mf' s = fst <$> mf s
                                          ma' s = fst <$> ma s

instance (Monad m) => Monad (StateT s m) where
    return = pure
    (StateT ma) >>= f = StateT $ \s -> do 
                            (a, _) <- ma s
                            (runStateT . f) a s
