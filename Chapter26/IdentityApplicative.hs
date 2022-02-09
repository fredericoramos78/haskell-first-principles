module Chapter26.IdentityApplicative where

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where 
    fmap f (Identity a) = Identity $ f a 

instance Applicative Identity where 
    pure = Identity 
    (<*>) (Identity f) a = f <$> a 

lmiApply :: [Maybe (Identity (a -> b))] -> [Maybe (Identity a)] -> [Maybe (Identity b)]
lmiApply f x = f2 <*> x 
                where f1 = (fmap . fmap) (<*>) f
                      f2 =  fmap (<*>) f1 