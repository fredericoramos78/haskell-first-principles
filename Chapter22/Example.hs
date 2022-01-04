module Chapter22.Example where

-- this is from https://williamyaoh.com/posts/2020-07-19-deriving-reader-monad.html

import Data.Char as Char

-- Our config data. We're very particular about certain
-- letters of the alphabet, see.
data ABConfig = ABConfig
  { don'tUseLetterE :: Bool
  , don'tUseLetterL :: Bool
  }

-- Uppercase the string, obeying current tests.
toUpperStr :: ABConfig -> String -> String
toUpperStr cfg str =
  filter passesFilters (fmap Char.toUpper str)
  where filters :: [Char -> Bool]
        filters =
          [ if don'tUseLetterE cfg then (/= 'E') else const True
          , if don'tUseLetterL cfg then (/= 'L') else const True
          ]

        passesFilters :: Char -> Bool
        passesFilters c = all (\f -> f c) filters

fullName :: ABConfig -> String -> String -> String -> String 
fullName cfg f l n = f' ++ " '" ++ n' ++ "' " ++ l' 
                    where f' = toUpperStr cfg f 
                          n' = toUpperStr cfg n
                          l' = toUpperStr cfg l 



-- 
data Reader cfg a = Reader { runReader :: cfg -> a }

instance Functor (Reader cfg) where 
      fmap f (Reader cfgF) = Reader (fmap f cfgF)

instance Applicative (Reader cfg) where 
    pure x = Reader (const x)
    (<*>) (Reader cfgX) (Reader cfgF) = Reader (cfgX <*> cfgF)
