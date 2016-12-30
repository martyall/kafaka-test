module Main where

import Site
import System.Environment (getArgs)

main :: IO ()
main = do
  fp <- head <$> getArgs
  startApp fp

