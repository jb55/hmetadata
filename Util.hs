
module Util where

import Data.Rated
import Data.Char (isSpace)

-- Utility functions
infixr 2 <?>
(<?>) :: Rated a -> a -> a
m1 <?> s = fromRated s m1


trim :: String -> String
trim = let f = reverse . dropWhile isSpace in f . f


maybeToRated :: Int -> Maybe a -> Rated a
maybeToRated i (Just t) = Rate i t
maybeToRated i Nothing  = Junk

