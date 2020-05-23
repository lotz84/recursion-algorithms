module Directory where

import Control.Monad (forM)

import System.Directory
import System.FilePath

-- | Manipulate subdirectories recursively to enumerate only files
listDirectoryRecursively :: FilePath -> IO [FilePath]
listDirectoryRecursively prepath = do
  ps <- listDirectory prepath
  fmap concat . forM ps $ \path -> do
    let fullpath = prepath </> path
    isDir <- doesDirectoryExist fullpath
    if isDir
       then listDirectoryRecursively fullpath
       else pure [fullpath]
