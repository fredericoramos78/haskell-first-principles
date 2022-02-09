{-# LANGUAGE InstanceSigs #-}
module Chapter26.MaybeTransformer where

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

-- NOTE: both `Functor` and `Applicative` have the exact same implementation
-- That's what it means to be `closed under composition`
instance (Functor m) => Functor (MaybeT m) where
    fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
    fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
    pure :: a -> MaybeT m a
    pure x = MaybeT $ (pure . pure) x

    (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
    (<*>) (MaybeT mf) (MaybeT ma) = MaybeT $ mf' <*> ma
                                    where --mf' :: m (Maybe a -> Maybe b) (see below)
                                          mf' = (<*>) <$> mf

    -- ma  == m (Maybe a)
    -- mf  == m (Maybe (a -> b))
    -- <*> == A (a -> b) -> A a -> A b
    -- to `<*>` on `ma` we need a `m (Maybe a -> Maybe b)`

    -- explanation for `mf'`
    -- `fmap` mf => exposes `Maybe (a -> b)` to be applied by the function
    -- <*> first arg is `A (a -> b)`, thus `A` == `Maybe`
    -- <*> `fmap` mf => `Maybe a -> Maybe b`

instance (Monad m) => Monad (MaybeT m) where
    return = pure

    (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    -- I forgot the `MaybeT $` portion which triggers a incomprehensible error! :(
    (>>=) (MaybeT ma) mf = MaybeT $ do
        maybeA <- ma
        case maybeA of 
            Just a -> runMaybeT $ mf a
            Nothing -> return Nothing
