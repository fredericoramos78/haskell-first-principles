module Chapter26.FixCode where

-- Fix the code
-- The code won’t type check as written; fix it so that it does. Feel free to add imports if they provide 
    -- something useful. Functions are used that we haven’t introduced. You’re not allowed to change the 
    -- types asserted. You may have to fix the code in more than one place:

import Control.Monad.Trans.Maybe
import Control.Monad

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = MaybeT $ do
    v <- getLine
    return $ if isValid v then Just v else Nothing

doExcite :: IO ()
doExcite = do
    putStrLn "say something excite!"
    excite <- runMaybeT maybeExcite
    case excite of
        Nothing -> putStrLn "MOAR EXCITE"
        Just e -> putStrLn ("Good, was very excite: " ++ e)
