module Utils (
  head'
) where

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x:xs) = Just x
