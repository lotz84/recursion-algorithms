module RemoveHs where

import Control.Monad (forM_, when)

import System.Directory (removeFile)
import System.FilePath (splitExtension)

import Directory (listDirectoryRecursively)

removeHs :: FilePath -> IO ()
removeHs dir = do
  ps <- listDirectoryRecursively dir
  forM_ ps $ \path ->
    let (filename, ext) = splitExtension path
     in when (ext == ".hs") $ removeFile path