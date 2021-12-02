module Identity where 

import Test.QuickCheck
import Data.Monoid

--
-- exercise 2: Identity data type
--
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where 
    (<>) (Identity a) (Identity b) = Identity (a <> b)

instance Monoid a => Monoid (Identity a) where 
    mempty = Identity $ mempty
    mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where 
    arbitrary = do
        a <- arbitrary 
        return $ Identity a