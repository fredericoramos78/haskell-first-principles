module Chapter11.Garden where

-- 1. Given the type:
data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show

type Gardener = String

data Garden =
  Garden Gardener FlowerType
  deriving Show

-- What is the sum of products normal form of Garden?
data Garden' = Gardener FlowerType 



data Fiction = FictionV deriving Show
data Nonfiction = NonfictionV deriving Show

data BookType = FictionBook Fiction 
              | NonfictionBook Nonfiction
              deriving Show

type AuthorName = String


data Author = Fiction AuthorName
            | Nonfiction AuthorName 
            deriving (Eq, Show)