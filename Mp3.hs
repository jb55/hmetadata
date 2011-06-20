{-# LANGUAGE OverloadedStrings #-}

import Data.List.Split
import Data.Char (isSpace)
import System.FilePath.Glob
import System.FilePath.Posix
import Data.List (intercalate)
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Data.Monoid
import Data.Maybe
import ID3.Simple
import System

import Debug.Trace

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

data MetaData = MetaData {
                  artist :: Rated String
                , title  :: Rated String
                } deriving (Show)

type Cleaner = FilePath -> IO MetaData

-- MetaData is a monoid
instance Monoid MetaData where
  (MetaData a1 t1) `mappend` (MetaData a2 t2) = MetaData (a1 <|> a2) (t1 <|> t2)
  mempty = MetaData Junk Junk

-- Utility functions
infixr 2 <?>
(<?>) :: Rated a -> a -> a
m1 <?> s = fromRated $ m1 <|> pure s

fmap' :: (Functor f1, Functor f) => (a -> b) -> f (f1 a) -> f (f1 b)
fmap' = fmap . fmap

trim = let f = reverse . dropWhile isSpace in f . f


-- Pretty print metadata
prettyMeta :: MetaData -> String
prettyMeta m@(MetaData artist title) = joined <?> show m
  where
    joined = joinStr " - " <$> unk artist <*> unk title
    joinStr sep a b = a ++ sep ++ b
    unk m = m <|> pure "Unknown"


-- Parse metadata from filename
filenameMeta :: FilePath -> MetaData
filenameMeta s = MetaData artist title
  where
    rate = Rate 1
    fileName = snd . splitFileName $ s
    dashSplit  = map trim $ splitOn "-" fileName
    stripMp3 name = case splitOn "." name of
                      []  -> name
                      [_] -> name
                      ss  -> intercalate "." . init $ ss
    (artist, title) = case dashSplit of
                        []       -> (Junk, Junk)
                        [t]      -> (Junk, rate . stripMp3 $ t)
                        [a, t]   -> (rate a, rate . stripMp3 $ t)
                        a:t:rest -> (rate a, rate t)

maybeToRated :: Int -> Maybe a -> Rated a
maybeToRated i (Just t) = Rate i t
maybeToRated i Nothing  = Junk

-- Convert an ID3Tag to MetaData
tagToMeta :: Tag -> MetaData
tagToMeta t = let rate = maybeToRated 10 in
              MetaData (rate . getArtist $ t)
                       (rate . getTitle $ t)

-- Parse metadata from id3
id3Meta :: FilePath -> IO MetaData
id3Meta file = do
  maybeTag <- readTag file
  return $ case maybeTag of
    Nothing -> mempty
    Just t  -> tagToMeta t


-- Get our data from many different sources
getMeta :: [Cleaner] -> FilePath -> IO MetaData
getMeta cleaners file = mconcat <$> (sequence $ map ($file) cleaners)

-- Main!
main = do
  args <- getArgs
  case args of
    []     -> print "usage: mp3 <path>"
    [path] -> go path
  where
    patterns = map compile ["*.mp3"]
    cleaners = [id3Meta, return . filenameMeta]
    go path = do
      files <- head . fst <$> globDir patterns path
      metas <- sequence $ map (getMeta cleaners) files
      mapM_ (print . prettyMeta) metas

