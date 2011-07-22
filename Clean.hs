
module Clean where

import Data.List.Split
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
import Util

data MetaData = MetaData {
                  artist :: Rated String
                , title  :: Rated String
                } deriving (Show)

data Song = Song {
            songFileName :: FilePath
          }

type Cleaner = Song -> IO MetaData

-- MetaData is a monoid
instance Monoid MetaData where
  (MetaData a1 t1) `mappend` (MetaData a2 t2) = MetaData (a1 <|> a2) (t1 <|> t2)
  mempty = MetaData Junk Junk



-- Pretty print metadata
prettyMeta :: MetaData -> String
prettyMeta m@(MetaData artist title) = joined <?> show m
  where
    joined = joinStr " - " <$> unk artist <*> unk title
    joinStr sep a b = a ++ sep ++ b
    unk m = m <|> pure "Unknown"


-- Parse metadata from filename
filenameMeta :: Song -> MetaData
filenameMeta s' = MetaData artist title
  where
    s          = songFileName s'
    rate       = Rate 1
    fileName   = snd . splitFileName $ s
    stripRate  = rate . stripMp3
    dashSplit  = map trim $ splitOn "-" fileName
    stripMp3 name = case splitOn "." name of
                      []  -> name
                      [s] -> name
                      ss  -> intercalate "." . init $ ss
    (artist, title) = case dashSplit of
                        []       -> (Junk, Junk)
                        [t]      -> (Junk, stripRate t)
                        [a, t]   -> (rate a, stripRate t)
                        a:t:rest -> (rate a, rate t)

-- Convert an ID3Tag to MetaData
tagToMeta :: Tag -> MetaData
tagToMeta t = let rate = (\f -> maybeToRated 10 . f $ t) in
              MetaData (rate getArtist)
                       (rate getTitle)

-- Parse metadata from id3
id3Meta :: Song -> IO MetaData
id3Meta song = readTag file >>= return . metaFromMaybeTag
  where
    file                      = songFileName song
    metaFromMaybeTag Nothing  = mempty
    metaFromMaybeTag (Just t) = tagToMeta t

-- Get our data from many different sources
getMeta :: [Cleaner] -> Song -> IO MetaData
getMeta cleaners song = mconcat <$> mapM ($song) cleaners

