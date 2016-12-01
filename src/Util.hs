module Util where


collapse :: (Maybe a, Maybe b) -> Maybe (a,b)
collapse (Just x, Just y) = Just (x, y)
collapse _                = Nothing

safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast (x:xs) = safeLast xs

safeTail :: [a] -> [a]
safeTail []     = []
safeTail (x:xs) = xs

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x
