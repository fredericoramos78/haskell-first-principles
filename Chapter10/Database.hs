module Chapter10.Database where 

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem] 
theDatabase =
    [ DbDate (UTCTime
        (fromGregorian 1911 5 1)
        (secondsToDiffTime 34123)
        ), 
      DbNumber 9001, 
      DbNumber 9003, 
      DbString "Hello, world!", 
      DbDate (UTCTime 
        (fromGregorian 1921 5 1)
        (secondsToDiffTime 34123)
      )
    ]

isDbDate :: DatabaseItem -> Bool 
isDbDate (DbDate _) = True
isDbDate _ = False

extractTime :: DatabaseItem -> UTCTime
extractTime (DbDate t) = t
extractTime _ = error "cannot extract time from non DbDate values"

-- 1. Write a function that filters for DbDate values and returns a list of the UTCTime values inside them:
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = map extractTime . filter isDbDate 

-- 2. Write a function that filters for DbNumber values and returns a list of the Integer values inside them:
isDbNumber :: DatabaseItem -> Bool 
isDbNumber (DbNumber _) = True
isDbNumber _ = False

extractInt :: DatabaseItem -> Integer 
extractInt (DbNumber t) = t
extractInt _ = error "cannot extract int from non DbNumber values"

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = map extractInt . filter isDbNumber

-- 3. Write a function that gets the most recent date:
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

-- 4. Write a function that sums all of the DbNumber values:
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

-- 5. Write a function that gets the average of the DbNumber values:
-- You'll probably need to use fromIntegral
-- to get from Integer to Double.
avgDb :: [DatabaseItem] -> Double
avgDb x = fromIntegral sumOfNumbers / fromIntegral lenOfNumbers
            where onlyNumbers = filterDbNumber x
                  sumOfNumbers = sum onlyNumbers 
                  lenOfNumbers = length onlyNumbers