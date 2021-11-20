module Chapter11.Cardinalities where

-- 1. data PugType = PugData
-- Cardinality: 1

-- 2. For this one, recall that Bool is also defined with the | symbol:
-- Cardinality: 3
data Airline =
    PapuAir
    | CatapultsR'Us
    | TakeYourChancesUnited

-- 3. Given what we know about Int8, what’s the cardinality of Int16?
-- Cardinality: 65536

-- 4. Use the REPL and maxBound and minBound to examine Int and Integer. 
--    What can you say about the cardinality of those types?
-- Int has cardinality of 2^4M while Integer has infinity cardinality

-- 5. Extra credit (impress your friends!): what’s the connection between the 8 in Int8 
-- and that type’s cardinality of 256?
-- 2^8 (=number of bits Int8 uses to store)