module Chapter26.Embeddeds where

import Control.Monad.Trans.Except 
import Control.Monad.Trans.Maybe 
import Control.Monad.Trans.Reader

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = MaybeT $ ExceptT $ return $ x ()
            where x = const (Right (Just 1))

-- breaking down
embedded' :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded' = do 
    let x :: a -> Either e (Maybe Int)
        x = const (Right (Just 1))
        w :: Either e (Maybe Int)
        -- Just to get the \_ -> Either e (Maybe Int) out of the way
        w = x ()
        --
        y :: ExceptT String (ReaderT () IO) (Maybe Int)
        y = ExceptT $ return w
        -- `ExceptT` waits for an error type `e`, and a successful value wrapped on a monad (=`m a`)
        --  `ExceptT e m a`, where `e == String`
        --                         `m == m` (still polymorphic), thus the need to type `y''`
        --                         `a == Maybe Int`
        z :: MaybeT (ExceptT String (ReaderT () IO)) Int
        --  `MaybeT` waits for a `m (Maybe a)` (which matches the `Right` type for the `ExceptT` above)
        --  `MaybeT m a`, where `m == ExceptT String (ReaderT () IO)`
        --                      `a == Int`
        z = MaybeT y
    z
