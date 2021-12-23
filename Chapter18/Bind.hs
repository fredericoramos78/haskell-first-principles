module Chapter18.Bind where

fmap' :: Functor f => (a -> b) -> f a -> f b
fmap' = undefined 

join' :: Monad m => m (m a) -> m a
join' = undefined 

bind :: Monad m => (a -> m b) -> m a -> m b 
bind mf = join' . fmap' mf
 