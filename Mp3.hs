
import Data.List.Split
import Data.List (intercalate)
import Data.Char (isSpace)
import System.FilePath.Glob
import System.FilePath.Posix
import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Maybe
import System

patterns = map compile ["*.mp3"]

data MetaData = MetaData {
                  artist :: Maybe String
                , title  :: Maybe String
                , reliability :: Int
                } deriving (Show)


-- Utility functions
infixr 2 <?>
(<?>) :: Maybe a -> a -> a
m1 <?> s = fromJust $ m1 <|> Just s

fmap' :: (Functor f1, Functor f) => (a -> b) -> f (f1 a) -> f (f1 b)
fmap' = fmap . fmap

trim = let f = reverse . dropWhile isSpace in f . f


-- Join metadata to improve results
joinMetaData :: MetaData -> MetaData -> MetaData
joinMetaData (MetaData a1 t1 r1) (MetaData a2 t2 r2)
    | r1 > r2   = MetaData (a1 <|> a2) (t1 <|> t2) r1
    | otherwise = MetaData (a2 <|> a1) (t2 <|> t1) r2


-- MetaData is a monoid
instance Monoid MetaData where
  mempty = MetaData Nothing Nothing 0
  mappend = joinMetaData


-- Pretty print metadata
prettyMeta :: MetaData -> String
prettyMeta m@(MetaData artist title _) = joined <?> show m
  where
    joined = joinStr " - " <$> unk artist <*> unk title
    joinStr sep a b = a ++ sep ++ b
    unk m = m <|> Just "Unknown"


-- Parse metadata from filename
filenameMeta :: String -> MetaData
filenameMeta s = MetaData artist title 1
  where
    dashSplit  = map trim $ splitOn "-" s
    stripMp3 s = case splitOn "." s of
                   []       -> s
                   _:[]     -> s
                   a@(x:xs) -> intercalate "." . init $ a
    (artist, title) = case dashSplit of
                        []       -> (Nothing, Nothing)
                        t:[]     -> (Nothing, Just . stripMp3 $ t)
                        a:t:[]   -> (Just a, Just . stripMp3 $ t)
                        a:t:rest -> (Just a, Just t)


-- Parse metadata from id3
id3Meta :: FilePath -> MetaData
id3Meta = undefined


-- Get our data from many different sources
getMeta :: FilePath -> MetaData
getMeta file = mconcat $ map ($file) [id3Meta, filenameMeta]


-- Main!
main = do
  args <- getArgs
  case args of
    []     -> print "usage: mp3 <path>"
    [path] -> go path
  where
    go    path       = join $ fmap (mapM_ printFile) (splitPaths . files $ path)
    printFile        = print . prettyMeta . filenameMeta
    files path       = fmap (head . fst) $ globDir patterns path
    splitPaths files = fmap' (snd . splitFileName) files


