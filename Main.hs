{-# LANGUAGE OverloadedStrings #-}

import System.FilePath.Glob
import System
import Control.Applicative

import Clean

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
      let songs = map Song files
      mapM (getMeta cleaners) songs >>= mapM_ printMeta

