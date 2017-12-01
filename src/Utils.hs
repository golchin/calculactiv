module Utils (
  head',
  trim
) where

import Data.List

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x:xs) = Just x

trim :: String -> String
trim v = (dropWhileEnd f . dropWhile f) v
  where f = (=='\n')
