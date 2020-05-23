
module Main where

import Test.DocTest

import Docs2Hs (docs2Hs)
import RemoveHs (removeHs)

main :: IO ()
main = do
    removeHs "docs"
    docs2Hs "docs"
    doctest ["-XBlockArguments", "-XLambdaCase", "docs"]
