{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM, forM_, when)
import Data.Maybe (catMaybes)

import CMarkGFM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
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


-- | Function to manipulate a Node recursively while transforming the NodeType
mapNode :: (NodeType -> a) -> Node -> [a]
mapNode f (Node _ nt []) = [f nt]
mapNode f (Node _ nt ns) = f nt : concatMap (mapNode f) ns


-- | Merge and return only the contents of the hs/haskell code block of Markdown text
-- If there is no corresponding code block, it returns Nothing
md2hs :: Text -> Maybe Text
md2hs = aggregate . catMaybes . mapNode getHsText . commonmarkToNode [] []
  where
    getHsText (CODE_BLOCK "hs"      code) = Just code
    getHsText (CODE_BLOCK "haskell" code) = Just code
    getHsText _                           = Nothing
    aggregate [] = Nothing
    aggregate ts = Just (T.unlines ts)


main :: IO ()
main = do
  ps <- listDirectoryRecursively "docs"
  forM_ ps $ \path ->
    let (filename, ext) = splitExtension path
     in when (ext == ".md") $ do
          code <- md2hs <$> T.readFile path
          case code of
            Nothing   -> pure ()
            Just code -> T.writeFile (filename ++ ".hs") code
