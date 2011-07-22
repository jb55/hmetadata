{-# LANGUAGE OverloadedStrings #-}

import Data.List.Split
import Data.Char (isSpace)
import System.FilePath.Glob
import System.FilePath.Posix
import Data.List (intercalate)
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Monoid
import Data.Maybe
import ID3.Simple
import System

import Data.Rated

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
                      [s] -> name
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
tagToMeta t = let rate = (\f -> maybeToRated 10 . f $ t) in
              MetaData (rate getArtist)
                       (rate getTitle)

-- Parse metadata from id3
id3Meta :: FilePath -> IO MetaData
id3Meta file = readTag file >>= return . metaFromMaybeTag
  where
    metaFromMaybeTag Nothing  = mempty
    metaFromMaybeTag (Just t) = tagToMeta t

-- Get our data from many different sources
getMeta :: [Cleaner] -> FilePath -> IO MetaData
getMeta cleaners file = mconcat <$> mapM ($file) cleaners

-- Main!
main = do
  args <- getArgs
  case args of
    []     -> print "usage: mp3 <path>"
    [path] -> go path
  where
    patterns = map compile ["*.mp3"]
    cleaners = [id3Meta, return . filenameMeta]
    printMeta = print . prettyMeta

    go path = do
      files <- head . fst <$> globDir patterns path
      mapM (getMeta cleaners) files >>= mapM_ printMeta

