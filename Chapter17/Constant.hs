module Chapter17.Constant where

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where 
    fmap f (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where 
    pure v = Constant mempty
    (<*>) (Constant x) (Constant y) = Constant y