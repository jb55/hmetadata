
module Data.Rated(
    Rated(Rate, Junk)
  , fromRated
  , changeRating
  , setRating
  , getRating
  ) where

import Control.Applicative

-- Rated
data Rated a = Rate Int a
             | Junk
             deriving (Show)

instance Eq (Rated a) where
  Junk == Junk = True
  Junk == _    = True
  (Rate r1 _) == (Rate r2 _) = r1 == r2

instance Ord (Rated a) where
  Junk `compare` Junk = EQ
  Junk `compare` _    = LT
  _    `compare` Junk = GT
  (Rate r1 _) `compare` (Rate r2 _)
    | r1 == r2  = EQ
    | r1 <= r2  = LT
    | otherwise = GT

instance Functor Rated where
  f `fmap` (Rate r a) = Rate r (f a)
  f `fmap` Junk = Junk

instance Applicative Rated where
  (Rate r1 f) <*> (Rate r2 x) = Rate r2 (f x)
  pure = Rate 0

instance Alternative Rated where
  Junk <|> Junk = Junk
  Junk <|> q    = q
  q    <|> Junk = q
  q1   <|> q2   = q1 `max` q2
  empty = Junk

instance Monad Rated where
  Junk >>= f = Junk
  m1@(Rate r1 a) >>= f = let m2@(Rate r2 b) = f a in
                         if r1 > r2 then (Rate r1 b) else m2
  return = pure

fromRated :: Rated a -> a
fromRated (Rate r a) = a

changeRating :: Rated a -> (Int -> Int) -> Rated a
changeRating (Rate r a) f = Rate (f r) a

setRating :: Rated a -> Int -> Rated a
setRating (Rate r a) i = Rate i a

getRating :: Rated a -> Int
getRating (Rate r _) = r
