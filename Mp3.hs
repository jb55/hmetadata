
import Data.List.Split
import Data.List (intercalate)
import Data.Char (isSpace)
import System.FilePath.Glob
import System.FilePath.Posix
import Control.Applicative
import Control.Monad
import Data.Maybe
import System

patterns = map compile ["*.mp3"]

data Metadata = Metadata {
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
joinMetadata :: Metadata -> Metadata -> Metadata
joinMetadata (m1 a1 t1 r1) (m2 a2 t2 r2)
    | r1 > r2   = Metadata (a1 <|> a2) (t1 <|> t2)
    | otherwise = Metadata (a2 <|> a1) (t2 <|> t1)


-- Metadata is a monoid
instance Monoid Metadata where
  mempty = Metadata Nothing Nothing
  mappend = joinMetadata


-- Pretty print metadata
prettyMeta :: Metadata -> String
prettyMeta m@(Metadata artist title) = joined <?> show m
  where
    joined = joinStr " - " <$> unk artist <*> unk title
    joinStr sep a b = a ++ sep ++ b
    unk m = m <|> Just "Unknown"


-- Parse metadata from filename
filenameMeta :: String -> Metadata
filenameMeta s = Metadata artist title 1
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
id3Meta :: FilePath -> Metadata
id3Meta = undefined


-- Get our data from many different sources
getMeta :: FilePath -> Metadata
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


