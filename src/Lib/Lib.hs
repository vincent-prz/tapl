module Lib.Lib where

lastMay :: [a] -> Maybe a
lastMay [] = Nothing
lastMay [x] = Just x
lastMay (_:xs) = lastMay xs

mapWithIndex :: (a -> Int -> b) -> [a] -> [b]
mapWithIndex f l = zipWith f l [0 ..]
