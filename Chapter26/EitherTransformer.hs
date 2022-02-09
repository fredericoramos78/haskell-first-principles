{-# LANGUAGE InstanceSigs #-}
module Chapter26.EitherTransformer where

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

-- NOTE: both `Functor` and `Applicative` have the exact same implementation
-- That's what it means to be `closed under composition`
instance Functor m => Functor (EitherT e m) where
    fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
    fmap f (EitherT ma) = EitherT $ (fmap . fmap) f ma

instance Applicative m => Applicative (EitherT e m) where
    pure a = EitherT $ (pure . pure) a
    (EitherT mf) <*> (EitherT ma) = EitherT $ mf' <*> ma
                                    where mf' = (<*>) <$> mf

instance Monad m => Monad (EitherT e m) where
    return = pure

    (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
    (>>=) (EitherT ma) mf = EitherT $ do
        -- ea :: Either e a
        ea <- ma
        case ea of
            Left l -> pure $ Left l
            Right a -> runEitherT . mf $ a

-- Write the swapEitherT helper function for EitherT:
-- Hint: write swapEither first, then write swapEitherT in terms of
-- the former.
swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ma) = EitherT $ swapEither <$> ma

swapEither :: Either e a -> Either a e 
swapEither (Right a) = Left a
swapEither (Left e) = Right e


-- 5. Write the transformer variant of the either catamorphism:
eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c 
eitherT fa fb (EitherT ma) = do 
    ea <- ma
    case ea of 
        Left a -> fa a
        Right b -> fb b

